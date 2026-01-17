"""CLI commands for Rainbow CLI."""

import importlib.metadata
import os
import platform
import shlex
import shutil
import ssl
import sys
from datetime import datetime
from pathlib import Path

import httpx
import typer
from rich.live import Live
from rich.panel import Panel
from rich.table import Table

from .config import AGENT_CONFIG, GITHUB_REPO_NAME, GITHUB_REPO_OWNER, SCRIPT_TYPE_CHOICES
from .github import _github_auth_headers, ssl_context
from .system_utils import check_tool, ensure_executable_scripts, init_git_repo, is_git_repo
from .templates import download_and_extract_template
from .ui import (
    StepTracker,
    app,
    console,
    multi_select_with_arrows,
    select_with_arrows,
    show_banner,
)

# Create HTTP client with SSL context
client = httpx.Client(verify=ssl_context)


@app.command()
def init(
    project_name: str = typer.Argument(None, help="Name for your new project directory (optional if using --here, or use '.' for current directory)"),
    ai_assistant: str = typer.Option(None, "--ai", help="AI agent(s) to use. Can be a single agent or comma-separated list (e.g., 'claude,gemini,copilot'). Valid options: claude, gemini, copilot, cursor-agent, qwen, opencode, codex, windsurf, kilocode, auggie, codebuddy, roo, amp, shai, q, bob, jules, qoder, antigravity. If not specified, an interactive multi-select menu will appear (default: copilot pre-selected)"),
    script_type: str = typer.Option(None, "--script", help="Script type to use: sh or ps"),
    ignore_agent_tools: bool = typer.Option(False, "--ignore-agent-tools", help="Skip checks for AI agent tools like Claude Code"),
    no_git: bool = typer.Option(False, "--no-git", help="Skip git repository initialization"),
    here: bool = typer.Option(False, "--here", help="Initialize project in the current directory instead of creating a new one"),
    force: bool = typer.Option(False, "--force", help="Force merge/overwrite when using --here (skip confirmation)"),
    upgrade: bool = typer.Option(False, "--upgrade", help="Upgrade existing Rainbow project by replacing .rainbow and agent folders with latest templates"),
    skip_tls: bool = typer.Option(False, "--skip-tls", help="Skip SSL/TLS verification (not recommended)"),
    debug: bool = typer.Option(False, "--debug", help="Show verbose diagnostic output for network and extraction failures"),
    github_token: str = typer.Option(None, "--github-token", help="GitHub token to use for API requests (or set GH_TOKEN or GITHUB_TOKEN environment variable)"),
    local_templates: bool = typer.Option(False, "--local-templates", help="Use local templates from repository instead of downloading from GitHub (for development)"),
    template_path: str = typer.Option(None, "--template-path", help="Path to local template directory (defaults to repo root if --local-templates is used)"),
):
    """
    Initialize a new Rainbow project from the latest template.

    This command will:
    1. Check that required tools are installed (git is optional)
    2. Let you choose your AI assistant(s) - supports multiple agents
    3. Download the appropriate template(s) from GitHub (or use local templates)
    4. Extract the template to a new project directory or current directory
    5. Initialize a fresh git repository (if not --no-git and no existing repo)
    6. Set up AI assistant commands for all selected agents

    Examples:
        # Basic project initialization (interactive multi-agent selection)
        rainbow init my-project

        # Initialize with specific AI assistant
        rainbow init my-project --ai claude

        # Initialize with multiple AI assistants (comma-separated)
        rainbow init my-project --ai claude,gemini,copilot

        # Initialize in current directory (interactive selection)
        rainbow init .
        rainbow init --here

        # Initialize with specific AI and script type
        rainbow init my-project --ai copilot --script ps

        # Multiple agents with script type
        rainbow init my-project --ai claude,cursor-agent --script sh

        # Force merge into current (non-empty) directory
        rainbow init . --force
        rainbow init --here --force

        # Upgrade existing project (prompts for AI selection)
        rainbow init --upgrade
        rainbow init --upgrade --ai claude
        rainbow init --here --upgrade

        # Use local templates for development
        rainbow init demo --local-templates --ai claude
        rainbow init demo --local-templates --template-path /path/to/hanoi-rainbow

        # With environment variables
        export RAINBOW_USE_LOCAL_TEMPLATES=1
        export RAINBOW_TEMPLATE_PATH=/path/to/hanoi-rainbow
        rainbow init demo --ai copilot
    """

    show_banner()

    # Check for environment variable to use local templates
    if not local_templates:
        local_templates_env = os.getenv("RAINBOW_USE_LOCAL_TEMPLATES", "").lower() in ("1", "true", "yes")
        if local_templates_env:
            local_templates = True
            console.print("[cyan]RAINBOW_USE_LOCAL_TEMPLATES detected - using local templates[/cyan]")

    # Check for template path from environment
    if template_path is None:
        env_template_path = os.getenv("RAINBOW_TEMPLATE_PATH")
        if env_template_path:
            template_path = env_template_path
            console.print(f"[cyan]Using RAINBOW_TEMPLATE_PATH: {template_path}[/cyan]")

    if project_name == ".":
        here = True
        project_name = None  # Clear project_name to use existing validation logic

    if here and project_name:
        console.print("[red]Error:[/red] Cannot specify both project name and --here flag")
        raise typer.Exit(1)

    if not here and not project_name and not upgrade:
        console.print("[red]Error:[/red] Must specify either a project name, use '.' for current directory, or use --here flag")
        raise typer.Exit(1)

    # For upgrade mode without explicit location, default to current directory
    if upgrade and not here and not project_name:
        here = True
        project_name = None

    # Track whether we're merging into an existing directory
    merge_into_existing = False

    if here:
        project_name = Path.cwd().name
        project_path = Path.cwd()

        existing_items = list(project_path.iterdir())
        if existing_items and not upgrade:
            console.print(f"[yellow]Warning:[/yellow] Current directory is not empty ({len(existing_items)} items)")
            console.print("[yellow]Template files will be merged with existing content and may overwrite existing files[/yellow]")
            if force:
                console.print("[cyan]--force supplied: skipping confirmation and proceeding with merge[/cyan]")
            else:
                response = typer.confirm("Do you want to continue?")
                if not response:
                    console.print("[yellow]Operation cancelled[/yellow]")
                    raise typer.Exit(0)
    else:
        project_path = Path(project_name).resolve()
        if project_path.exists() and not upgrade:
            # Check if Rainbow is already installed in this directory
            rainbow_dir = project_path / ".rainbow"
            if rainbow_dir.exists():
                # Rainbow is installed, suggest upgrade
                error_panel = Panel(
                    f"Directory '[cyan]{project_name}[/cyan]' already exists with Rainbow installed\n\n"
                    "To upgrade the existing Rainbow installation, use:\n"
                    f"  [cyan]rainbow init {project_name} --upgrade[/cyan]\n\n"
                    "Or choose a different project name.",
                    title="[yellow]Rainbow Already Installed[/yellow]",
                    border_style="yellow",
                    padding=(1, 2)
                )
                console.print()
                console.print(error_panel)
                raise typer.Exit(1)
            else:
                # Directory exists but no Rainbow installation
                error_panel = Panel(
                    f"Directory '[cyan]{project_name}[/cyan]' already exists\n\n"
                    "Rainbow is not installed in this directory. You can:\n"
                    "  • Use [cyan]--force[/cyan] to merge Rainbow files into the existing directory\n"
                    f"  • Use [cyan]rainbow init . --force[/cyan] from within the directory\n"
                    "  • Choose a different project name",
                    title="[yellow]Directory Already Exists[/yellow]",
                    border_style="yellow",
                    padding=(1, 2)
                )
                console.print()
                console.print(error_panel)

                # Ask user if they want to proceed with force
                if not force and sys.stdin.isatty():
                    response = typer.confirm("\nDo you want to proceed and merge Rainbow files into this directory?")
                    if response:
                        console.print("[cyan]Proceeding with merge into existing directory[/cyan]")
                        merge_into_existing = True
                        # Continue with installation
                    else:
                        console.print("[yellow]Operation cancelled[/yellow]")
                        raise typer.Exit(0)
                else:
                    if force:
                        console.print("[cyan]--force supplied: proceeding with merge into existing directory[/cyan]")
                        merge_into_existing = True
                    else:
                        raise typer.Exit(1)

    current_dir = Path.cwd()

    # Upgrade mode validation and setup
    is_upgrade_mode = False
    backup_paths = {}
    existing_agents = []

    if upgrade:
        rainbow_dir = project_path / ".rainbow"
        if not rainbow_dir.exists():
            error_panel = Panel(
                f"No Rainbow project found in '[cyan]{project_path}[/cyan]'\n"
                "The .rainbow folder does not exist. Cannot upgrade.\n\n"
                "Use 'rainbow init' without --upgrade to initialize a new project.",
                title="[red]Upgrade Failed[/red]",
                border_style="red",
                padding=(1, 2)
            )
            console.print()
            console.print(error_panel)
            raise typer.Exit(1)

        # Detect existing agent folders
        for agent_key, agent_config in AGENT_CONFIG.items():
            agent_folder = project_path / agent_config["agent_folder"]
            if agent_folder.exists():
                existing_agents.append((agent_key, agent_config["name"], agent_config["agent_folder"]))

        is_upgrade_mode = True

        # Show upgrade warning
        upgrade_lines = [
            "[yellow]⚠️  Upgrade Mode[/yellow]\n",
            "The following will be [bold red]completely replaced[/bold red]:",
            "  • .rainbow/ folder (scripts, templates, memory)",
        ]

        if existing_agents:
            upgrade_lines.append("  • Detected agent folders:")
            for _, agent_name, agent_folder in existing_agents:
                upgrade_lines.append(f"    - {agent_folder}")

        upgrade_lines.extend([
            "",
            "[cyan]Backups will be created with timestamp.[/cyan]",
            "[dim]User content (docs/, project files) will be preserved.[/dim]"
        ])

        console.print()
        console.print(Panel("\n".join(upgrade_lines), border_style="yellow", padding=(1, 2)))

        if not force:
            response = typer.confirm("\nDo you want to continue with the upgrade?")
            if not response:
                console.print("[yellow]Upgrade cancelled[/yellow]")
                raise typer.Exit(0)

    setup_lines = [
        f"[cyan]Rainbow Project {'Upgrade' if is_upgrade_mode else 'Setup'}[/cyan]",
        "",
        f"{'Project':<15} [green]{project_path.name}[/green]",
        f"{'Working Path':<15} [dim]{current_dir}[/dim]",
    ]

    if not here or merge_into_existing:
        setup_lines.append(f"{'Target Path':<15} [dim]{project_path}[/dim]")

    if is_upgrade_mode:
        setup_lines.append(f"{'Mode':<15} [yellow]UPGRADE[/yellow]")
    elif merge_into_existing:
        setup_lines.append(f"{'Mode':<15} [yellow]MERGE[/yellow]")

    console.print(Panel("\n".join(setup_lines), border_style="cyan", padding=(1, 2)))

    should_init_git = False
    if not no_git:
        should_init_git = check_tool("git")
        if not should_init_git:
            console.print("[yellow]Git not found - will skip repository initialization[/yellow]")

    if ai_assistant:
        # Parse comma-separated list of AI assistants
        selected_ais = [ai.strip() for ai in ai_assistant.split(',')]

        # Validate each AI assistant
        invalid_ais = [ai for ai in selected_ais if ai not in AGENT_CONFIG]
        if invalid_ais:
            console.print(f"[red]Error:[/red] Invalid AI assistant(s): {', '.join(invalid_ais)}")
            console.print(f"[yellow]Valid options:[/yellow] {', '.join(AGENT_CONFIG.keys())}")
            raise typer.Exit(1)
    else:
        # Create options dict for selection (agent_key: display_name)
        ai_choices = {key: config["name"] for key, config in AGENT_CONFIG.items()}
        selected_ais = multi_select_with_arrows(
            ai_choices,
            "Choose your AI assistants (Space to toggle, Enter to confirm):",
            ["copilot"]
        )

    if not ignore_agent_tools:
        for selected_ai in selected_ais:
            agent_config = AGENT_CONFIG.get(selected_ai)
            if agent_config and agent_config["requires_cli"]:
                install_url = agent_config["install_url"]
                if not check_tool(selected_ai):
                    error_panel = Panel(
                        f"[cyan]{selected_ai}[/cyan] not found\n"
                        f"Install from: [cyan]{install_url}[/cyan]\n"
                        f"{agent_config['name']} is required to continue with this project type.\n\n"
                        "Tip: Use [cyan]--ignore-agent-tools[/cyan] to skip this check",
                        title="[red]Agent Detection Error[/red]",
                        border_style="red",
                        padding=(1, 2)
                    )
                    console.print()
                    console.print(error_panel)
                    raise typer.Exit(1)

    if script_type:
        if script_type not in SCRIPT_TYPE_CHOICES:
            console.print(f"[red]Error:[/red] Invalid script type '{script_type}'. Choose from: {', '.join(SCRIPT_TYPE_CHOICES.keys())}")
            raise typer.Exit(1)
        selected_script = script_type
    else:
        default_script = "ps" if os.name == "nt" else "sh"

        if sys.stdin.isatty():
            selected_script = select_with_arrows(SCRIPT_TYPE_CHOICES, "Choose script type (or press Enter)", default_script)
        else:
            selected_script = default_script

    console.print(f"[cyan]Selected AI assistant(s):[/cyan] {', '.join(selected_ais)}")
    console.print(f"[cyan]Selected script type:[/cyan] {selected_script}")

    tracker = StepTracker("Upgrade Rainbow Project" if is_upgrade_mode else "Initialize Rainbow Project")

    sys._specify_tracker_active = True

    tracker.add("precheck", "Check required tools")
    tracker.complete("precheck", "ok")
    tracker.add("ai-select", "Select AI assistant(s)")
    tracker.complete("ai-select", f"{', '.join(selected_ais)}")
    tracker.add("script-select", "Select script type")
    tracker.complete("script-select", selected_script)

    # Add backup step for upgrade mode
    if is_upgrade_mode:
        tracker.add("backup", "Backup existing files")

    # Add steps for each AI assistant template download (only if not using local templates)
    if not local_templates:
        for selected_ai in selected_ais:
            for key, label in [
                (f"fetch-{selected_ai}", f"Fetch {selected_ai} release"),
                (f"download-{selected_ai}", f"Download {selected_ai} template"),
                (f"extract-{selected_ai}", f"Extract {selected_ai} template"),
            ]:
                tracker.add(key, label)
    else:
        # For local templates, simpler steps
        for selected_ai in selected_ais:
            tracker.add(f"copy-{selected_ai}", f"Copy {selected_ai} templates")

    # Add common steps
    for key, label in [
        ("chmod", "Ensure scripts executable"),
        ("cleanup", "Cleanup"),
        ("git", "Initialize git repository"),
        ("final", "Finalize")
    ]:
        tracker.add(key, label)

    # Track git error message outside Live context so it persists
    git_error_message = None

    with Live(tracker.render(), console=console, refresh_per_second=8, transient=True) as live:
        tracker.attach_refresh(lambda: live.update(tracker.render()))
        try:
            verify = not skip_tls
            local_ssl_context = ssl_context if verify else False
            local_client = httpx.Client(verify=local_ssl_context)

            # Perform backup in upgrade mode
            if is_upgrade_mode:
                tracker.start("backup")
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

                # Backup .rainbow folder
                rainbow_dir = project_path / ".rainbow"
                if rainbow_dir.exists():
                    backup_rainbow = project_path / f".rainbow.backup.{timestamp}"
                    shutil.copytree(rainbow_dir, backup_rainbow)
                    backup_paths[".rainbow"] = backup_rainbow

                # Backup Jules root-level skills folder (special case)
                jules_skills = project_path / "skills"
                if jules_skills.exists():
                    backup_jules_skills = project_path / f"skills.backup.{timestamp}"
                    shutil.copytree(jules_skills, backup_jules_skills)
                    backup_paths["skills"] = backup_jules_skills

                # Backup existing agent folders
                for agent_key, agent_name, agent_folder in existing_agents:
                    source_folder = project_path / agent_folder
                    if source_folder.exists():
                        # Remove trailing slash for backup name
                        folder_name = agent_folder.rstrip('/')
                        backup_folder = project_path / f"{folder_name}.backup.{timestamp}"
                        shutil.copytree(source_folder, backup_folder)
                        backup_paths[agent_folder] = backup_folder

                backup_count = len(backup_paths)
                tracker.complete("backup", f"{backup_count} folder{'s' if backup_count != 1 else ''} backed up")

            # Download and extract templates for each selected AI agent
            # Only copy shared .rainbow folder for the first agent to avoid redundancy
            for idx, selected_ai in enumerate(selected_ais):
                is_first = (idx == 0)
                download_and_extract_template(
                    project_path, selected_ai, selected_script, here or merge_into_existing,
                    verbose=False, tracker=tracker, client=local_client,
                    debug=debug, github_token=github_token,
                    local_templates=local_templates, template_path=template_path,
                    is_first_agent=is_first
                )

            ensure_executable_scripts(project_path, tracker=tracker)

            if not no_git:
                tracker.start("git")
                if is_git_repo(project_path):
                    tracker.complete("git", "existing repo detected")
                elif should_init_git:
                    success, error_msg = init_git_repo(project_path, quiet=True)
                    if success:
                        tracker.complete("git", "initialized")
                    else:
                        tracker.error("git", "init failed")
                        git_error_message = error_msg
                else:
                    tracker.skip("git", "git not available")
            else:
                tracker.skip("git", "--no-git flag")

            tracker.complete("final", "project ready")
        except Exception as e:
            tracker.error("final", str(e))
            console.print(Panel(f"Initialization failed: {e}", title="Failure", border_style="red"))
            if debug:
                _env_pairs = [
                    ("Python", sys.version.split()[0]),
                    ("Platform", sys.platform),
                    ("CWD", str(Path.cwd())),
                ]
                _label_width = max(len(k) for k, _ in _env_pairs)
                env_lines = [f"{k.ljust(_label_width)} → [bright_black]{v}[/bright_black]" for k, v in _env_pairs]
                console.print(Panel("\n".join(env_lines), title="Debug Environment", border_style="magenta"))
            if not here and project_path.exists():
                shutil.rmtree(project_path)
            raise typer.Exit(1)
        finally:
            pass

    console.print(tracker.render())
    console.print("\n[bold green]Project ready.[/bold green]")

    # Show backup information for upgrade mode
    if is_upgrade_mode and backup_paths:
        console.print()
        backup_lines = ["[cyan]Backups created:[/cyan]"]
        for original, backup in backup_paths.items():
            backup_lines.append(f"  • {original} → {backup.name}")
        backup_lines.append("\n[dim]To restore from backup, remove the current folder and rename the backup.[/dim]")
        backup_panel = Panel(
            "\n".join(backup_lines),
            title="[cyan]Backup Information[/cyan]",
            border_style="cyan",
            padding=(1, 2)
        )
        console.print(backup_panel)

    # Show git error details if initialization failed
    if git_error_message:
        console.print()
        git_error_panel = Panel(
            f"[yellow]Warning:[/yellow] Git repository initialization failed\n\n"
            f"{git_error_message}\n\n"
            f"[dim]You can initialize git manually later with:[/dim]\n"
            f"[cyan]cd {project_path if not here else '.'}[/cyan]\n"
            f"[cyan]git init[/cyan]\n"
            f"[cyan]git add .[/cyan]\n"
            f"[cyan]git commit -m \"Initial commit\"[/cyan]",
            title="[red]Git Initialization Failed[/red]",
            border_style="red",
            padding=(1, 2)
        )
        console.print(git_error_panel)

    # Agent folder security notice
    agent_config = AGENT_CONFIG.get(selected_ais[-1])  # Use last selected AI for the notice
    if agent_config:
        agent_folder = agent_config["agent_folder"]
        security_notice = Panel(
            f"Some agents may store credentials, auth tokens, or other identifying and private artifacts in the agent folder within your project.\n"
            f"Consider adding [cyan]{agent_folder}[/cyan] (or parts of it) to [cyan].gitignore[/cyan] to prevent accidental credential leakage.",
            title="[yellow]Agent Folder Security[/yellow]",
            border_style="yellow",
            padding=(1, 2)
        )
        console.print()
        console.print(security_notice)

    steps_lines = []
    if not here and not merge_into_existing:
        steps_lines.append(f"1. Go to the project folder: [cyan]cd {project_name}[/cyan]")
        step_num = 2
    else:
        if here:
            steps_lines.append("1. You're already in the project directory!")
        else:
            steps_lines.append(f"1. Go to the project folder: [cyan]cd {project_name}[/cyan]")
        step_num = 2

    # Add Codex-specific setup step if needed
    if "codex" in selected_ais:
        codex_path = project_path / ".codex"
        quoted_path = shlex.quote(str(codex_path))
        if os.name == "nt":  # Windows
            cmd = f"setx CODEX_HOME {quoted_path}"
        else:  # Unix-like systems
            cmd = f"export CODEX_HOME={quoted_path}"

        steps_lines.append(f"{step_num}. Set [cyan]CODEX_HOME[/cyan] environment variable before running Codex: [cyan]{cmd}[/cyan]")
        step_num += 1

    steps_lines.append(f"{step_num}. Start using slash commands with your AI agent (in order):")
    steps_lines.append("")
    steps_lines.append("   [bold cyan]Core Workflow:[/bold cyan]")
    steps_lines.append("   • [cyan]/rainbow.regulate[/] - Establish project principles (ground rules)")
    steps_lines.append("   • [cyan]/rainbow.specify[/] - Create feature specification")
    steps_lines.append("   • [cyan]/rainbow.architect[/] [dim](optional, product-level)[/dim] - Define system architecture")
    steps_lines.append("   • [cyan]/rainbow.standardize[/] [dim](optional, product-level)[/dim] - Establish coding standards")
    steps_lines.append("   • [cyan]/rainbow.design[/] - Create technical implementation plan")
    steps_lines.append("   • [cyan]/rainbow.taskify[/] - Generate actionable tasks")
    steps_lines.append("   • [cyan]/rainbow.implement[/] - Execute implementation")

    steps_panel = Panel("\n".join(steps_lines), title="Next Steps", border_style="cyan", padding=(1,2))
    console.print()
    console.print(steps_panel)

    enhancement_lines = [
        "Optional commands for quality & validation [bright_black](use as needed)[/bright_black]",
        "",
        f"○ [cyan]/rainbow.clarify[/] - Ask structured questions before planning (run before [cyan]/rainbow.design[/])",
        f"○ [cyan]/rainbow.analyze[/] - Cross-artifact consistency report (after [cyan]/rainbow.taskify[/], before [cyan]/rainbow.implement[/])",
        f"○ [cyan]/rainbow.checklist[/] - Generate quality checklists (after [cyan]/rainbow.design[/])",
        f"○ [cyan]/rainbow.design-e2e-test[/] - Design end-to-end test specs (product-level)",
        f"○ [cyan]/rainbow.perform-e2e-test[/] - Execute E2E tests and generate reports"
    ]
    enhancements_panel = Panel("\n".join(enhancement_lines), title="Quality & Enhancement Commands", border_style="cyan", padding=(1,2))
    console.print()
    console.print(enhancements_panel)


@app.command()
def check():
    """Check that all required tools are installed."""
    show_banner()
    console.print("[bold]Checking for installed tools...[/bold]\n")

    tracker = StepTracker("Check Available Tools")

    tracker.add("git", "Git version control")
    git_ok = check_tool("git", tracker=tracker)

    agent_results = {}
    for agent_key, agent_config in AGENT_CONFIG.items():
        agent_name = agent_config["name"]
        requires_cli = agent_config["requires_cli"]

        tracker.add(agent_key, agent_name)

        if requires_cli:
            agent_results[agent_key] = check_tool(agent_key, tracker=tracker)
        else:
            # IDE-based agent - skip CLI check and mark as optional
            tracker.skip(agent_key, "IDE-based, no CLI check")
            agent_results[agent_key] = False  # Don't count IDE agents as "found"

    # Check VS Code variants (not in agent config)
    tracker.add("code", "Visual Studio Code")
    code_ok = check_tool("code", tracker=tracker)

    tracker.add("code-insiders", "Visual Studio Code Insiders")
    code_insiders_ok = check_tool("code-insiders", tracker=tracker)

    console.print(tracker.render())

    console.print("\n[bold green]Rainbow CLI is ready to use![/bold green]")

    if not git_ok:
        console.print("[dim]Tip: Install git for repository management[/dim]")

    if not any(agent_results.values()):
        console.print("[dim]Tip: Install an AI assistant for the best experience[/dim]")


@app.command()
def version():
    """Display version and system information."""

    show_banner()

    # Get CLI version from package metadata
    cli_version = "unknown"
    try:
        cli_version = importlib.metadata.version("rainbow-cli")
    except Exception:
        # Fallback: try reading from pyproject.toml if running from source
        try:
            import tomllib
            pyproject_path = Path(__file__).parent.parent.parent / "pyproject.toml"
            if pyproject_path.exists():
                with open(pyproject_path, "rb") as f:
                    data = tomllib.load(f)
                    cli_version = data.get("project", {}).get("version", "unknown")
        except Exception:
            pass

    # Fetch latest template release version
    api_url = f"https://api.github.com/repos/{GITHUB_REPO_OWNER}/{GITHUB_REPO_NAME}/releases/latest"

    template_version = "unknown"
    release_date = "unknown"

    try:
        response = client.get(
            api_url,
            timeout=10,
            follow_redirects=True,
            headers=_github_auth_headers(),
        )
        if response.status_code == 200:
            release_data = response.json()
            template_version = release_data.get("tag_name", "unknown")
            # Remove 'v' prefix if present
            if template_version.startswith("v"):
                template_version = template_version[1:]
            release_date = release_data.get("published_at", "unknown")
            if release_date != "unknown":
                # Format the date nicely
                try:
                    dt = datetime.fromisoformat(release_date.replace('Z', '+00:00'))
                    release_date = dt.strftime("%Y-%m-%d")
                except Exception:
                    pass
    except Exception:
        pass

    info_table = Table(show_header=False, box=None, padding=(0, 2))
    info_table.add_column("Key", style="cyan", justify="right")
    info_table.add_column("Value", style="white")

    info_table.add_row("CLI Version", cli_version)
    info_table.add_row("Template Version", template_version)
    info_table.add_row("Released", release_date)
    info_table.add_row("", "")
    info_table.add_row("Python", platform.python_version())
    info_table.add_row("Platform", platform.system())
    info_table.add_row("Architecture", platform.machine())
    info_table.add_row("OS Version", platform.version())

    panel = Panel(
        info_table,
        title="[bold cyan]Rainbow CLI Information[/bold cyan]",
        border_style="cyan",
        padding=(1, 2)
    )

    console.print(panel)
    console.print()
