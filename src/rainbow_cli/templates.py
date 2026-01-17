"""Template operations for Rainbow CLI."""

import json
import shutil
import tempfile
import zipfile
from pathlib import Path
from typing import TYPE_CHECKING

import httpx

from .config import AGENT_CONFIG, ARGS_FORMAT_MAP, EXTENSION_MAP
from .github import download_template_from_github
from .ui import console

if TYPE_CHECKING:
    from .ui import StepTracker


def handle_vscode_settings(sub_item, dest_file, rel_path, verbose=False, tracker=None) -> None:
    """Handle merging or copying of .vscode/settings.json files."""
    def log(message, color="green"):
        if verbose and not tracker:
            console.print(f"[{color}]{message}[/] {rel_path}")

    try:
        with open(sub_item, 'r', encoding='utf-8') as f:
            new_settings = json.load(f)

        if dest_file.exists():
            merged = merge_json_files(dest_file, new_settings, verbose=verbose and not tracker)
            with open(dest_file, 'w', encoding='utf-8') as f:
                json.dump(merged, f, indent=4)
                f.write('\n')
            log("Merged:", "green")
        else:
            shutil.copy2(sub_item, dest_file)
            log("Copied (no existing settings.json):", "blue")

    except Exception as e:
        log(f"Warning: Could not merge, copying instead: {e}", "yellow")
        shutil.copy2(sub_item, dest_file)


def merge_json_files(existing_path: Path, new_content: dict, verbose: bool = False) -> dict:
    """Merge new JSON content into existing JSON file.

    Performs a deep merge where:
    - New keys are added
    - Existing keys are preserved unless overwritten by new content
    - Nested dictionaries are merged recursively
    - Lists and other values are replaced (not merged)

    Args:
        existing_path: Path to existing JSON file
        new_content: New JSON content to merge in
        verbose: Whether to print merge details

    Returns:
        Merged JSON content as dict
    """
    try:
        with open(existing_path, 'r', encoding='utf-8') as f:
            existing_content = json.load(f)
    except (FileNotFoundError, json.JSONDecodeError):
        # If file doesn't exist or is invalid, just use new content
        return new_content

    def deep_merge(base: dict, update: dict) -> dict:
        """Recursively merge update dict into base dict."""
        result = base.copy()
        for key, value in update.items():
            if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                # Recursively merge nested dictionaries
                result[key] = deep_merge(result[key], value)
            else:
                # Add new key or replace existing value
                result[key] = value
        return result

    merged = deep_merge(existing_content, new_content)

    if verbose:
        console.print(f"[cyan]Merged JSON file:[/cyan] {existing_path.name}")

    return merged


def copy_local_template(
    project_path: Path,
    source_path: Path,
    ai_assistant: str,
    script_type: str,
    is_current_dir: bool = False,
    verbose: bool = True,
    tracker: "StepTracker | None" = None,
    is_first_agent: bool = True
) -> Path:
    """Copy local template files to the project directory.

    Args:
        is_first_agent: If True, copies shared .rainbow folder. If False, skips it.
                       This prevents redundant copying when multiple AI assistants are selected.
    """

    # Paths to copy
    commands_dir = source_path / "commands"
    skills_dir = source_path / "skills"
    memory_dir = source_path / "memory"
    scripts_dir = source_path / "scripts"

    # Check if required directories exist
    if not commands_dir.exists():
        raise FileNotFoundError(f"Commands directory not found: {commands_dir}")

    if verbose and not tracker:
        console.print(f"[cyan]Copying templates from:[/cyan] {source_path}")

    # Create project directory if needed
    if not is_current_dir:
        project_path.mkdir(parents=True, exist_ok=True)

    # Determine target structure based on AI assistant
    agent_config = AGENT_CONFIG.get(ai_assistant)
    if not agent_config:
        raise ValueError(f"Unknown AI assistant: {ai_assistant}")

    agent_folder = agent_config["agent_folder"]
    skills_folder = agent_config["skills_folder"]

    agent_ext = EXTENSION_MAP.get(ai_assistant, ".md")
    args_format = ARGS_FORMAT_MAP.get(ai_assistant, "$ARGUMENTS")

    # Only copy shared .rainbow folder for the first AI assistant
    # This prevents redundant copying when multiple AI assistants are selected
    if is_first_agent:
        # Create .rainbow directory structure
        rainbow_dir = project_path / ".rainbow"
        rainbow_dir.mkdir(exist_ok=True)

        # Copy memory
        if memory_dir.exists():
            dest_memory = rainbow_dir / "memory"
            if dest_memory.exists():
                shutil.rmtree(dest_memory)
            shutil.copytree(memory_dir, dest_memory)
            if verbose and not tracker:
                console.print(f"[green]✓[/green] Copied memory")

        # Copy scripts (filter by script_type)
        if scripts_dir.exists():
            dest_scripts = rainbow_dir / "scripts"
            dest_scripts.mkdir(exist_ok=True)

            # Copy common files
            for item in scripts_dir.iterdir():
                if item.is_file():
                    shutil.copy2(item, dest_scripts / item.name)

            # Copy script variant directory
            if script_type == "sh" and (scripts_dir / "bash").exists():
                shutil.copytree(scripts_dir / "bash", dest_scripts / "bash", dirs_exist_ok=True)
            elif script_type == "ps" and (scripts_dir / "powershell").exists():
                shutil.copytree(scripts_dir / "powershell", dest_scripts / "powershell", dirs_exist_ok=True)

            if verbose and not tracker:
                console.print(f"[green]✓[/green] Copied scripts ({script_type})")

        # Copy templates to .rainbow/templates with subdirectories
        dest_templates = rainbow_dir / "templates"
        dest_templates.mkdir(exist_ok=True)

        # Copy from commands/templates-for-commands to .rainbow/templates/templates-for-commands
        cmd_templates = commands_dir / "templates-for-commands"
        if cmd_templates.exists():
            dest_cmd_templates = dest_templates / "templates-for-commands"
            if dest_cmd_templates.exists():
                shutil.rmtree(dest_cmd_templates)
            shutil.copytree(cmd_templates, dest_cmd_templates)

        if verbose and not tracker:
            console.print(f"[green]✓[/green] Copied templates")

    # Create agent-specific command directory
    agent_path = project_path / agent_folder
    agent_path.mkdir(parents=True, exist_ok=True)

    # Copy command files to agent directory
    if commands_dir.exists():
        for cmd_file in commands_dir.glob("*.md"):
            # Read the command file
            content = cmd_file.read_text()

            # Replace placeholders
            content = content.replace("$ARGUMENTS", args_format)
            content = content.replace("__AGENT__", ai_assistant)

            # Write to agent directory with appropriate extension
            output_file = agent_path / f"rainbow.{cmd_file.stem}{agent_ext}"
            # Only write if file doesn't exist or content is different (handles shared folders)
            if not output_file.exists() or output_file.read_text() != content:
                output_file.write_text(content)

    if verbose and not tracker:
        console.print(f"[green]✓[/green] Created {ai_assistant} commands in {agent_folder}")

    # For GitHub Copilot, also copy to prompts folder
    prompts_folder = agent_config.get("prompts_folder")
    if prompts_folder and commands_dir.exists():
        prompts_path = project_path / prompts_folder
        prompts_path.mkdir(parents=True, exist_ok=True)
        
        for cmd_file in commands_dir.glob("*.md"):
            # Read the command file
            content = cmd_file.read_text()

            # Replace placeholders
            content = content.replace("$ARGUMENTS", args_format)
            content = content.replace("__AGENT__", ai_assistant)

            # Write to prompts directory with .prompt.md extension
            output_file = prompts_path / f"rainbow.{cmd_file.stem}.prompt.md"
            # Only write if file doesn't exist or content is different (handles shared folders)
            if not output_file.exists() or output_file.read_text() != content:
                output_file.write_text(content)
        
        if verbose and not tracker:
            console.print(f"[green]✓[/green] Created {ai_assistant} prompts in {prompts_folder}")

    # Copy skills to agent-specific skills folder
    if skills_dir.exists():
        skills_path = project_path / skills_folder
        skills_path.mkdir(parents=True, exist_ok=True)

        # Copy all skills subdirectories
        # Use dirs_exist_ok=True to handle cases where multiple agents share the same skills folder
        for skill_item in skills_dir.iterdir():
            if skill_item.is_dir():
                dest_skill = skills_path / skill_item.name
                # Merge directories instead of removing them when multiple agents share the same folder
                shutil.copytree(skill_item, dest_skill, dirs_exist_ok=True)

        if verbose and not tracker:
            console.print(f"[green]✓[/green] Created {ai_assistant} skills in {skills_folder}")

    # 
    # Handle agent-specific files (e.g., vscode-settings.json for copilot)
    if ai_assistant == "copilot":
        # Create .vscode/settings.json
        vscode_dir = project_path / ".vscode"
        vscode_dir.mkdir(exist_ok=True)
        vscode_settings_src = commands_dir / "templates-for-commands" / "vscode-settings.json"
        if vscode_settings_src.exists():
            shutil.copy2(vscode_settings_src, vscode_dir / "settings.json")
            if verbose and not tracker:
                console.print(f"[green]✓[/green] Created .vscode/settings.json")

    if tracker:
        tracker.complete(f"copy-{ai_assistant}", "templates copied")

    return project_path


def download_and_extract_template(
    project_path: Path,
    ai_assistant: str,
    script_type: str,
    is_current_dir: bool = False,
    *,
    verbose: bool = True,
    tracker: "StepTracker | None" = None,
    client: httpx.Client = None,
    debug: bool = False,
    github_token: str = None,
    local_templates: bool = False,
    template_path: str = None,
    is_first_agent: bool = True
) -> Path:
    """Download the latest release and extract it to create a new project.
    Returns project_path. Uses tracker if provided (with keys: fetch, download, extract, cleanup)
    If local_templates is True, copies from local template_path instead of downloading.

    Args:
        is_first_agent: If True, copies shared .rainbow folder. If False, skips it to avoid redundancy.
    """
    current_dir = Path.cwd()

    # Handle local templates
    if local_templates:
        if tracker:
            tracker.start(f"copy-{ai_assistant}")

        # Determine template source path
        if template_path:
            source_path = Path(template_path).resolve()
        else:
            # Default to repo root (assume we're in src/rainbow_cli)
            source_path = Path(__file__).parent.parent.parent.resolve()

        if not source_path.exists():
            error_msg = f"Template path does not exist: {source_path}"
            if tracker:
                tracker.error(f"copy-{ai_assistant}", error_msg)
            raise FileNotFoundError(error_msg)

        if verbose and not tracker:
            console.print(f"[cyan]Using local templates from:[/cyan] {source_path}")

        # Build the template by creating a structure similar to the release package
        return copy_local_template(project_path, source_path, ai_assistant, script_type, is_current_dir, verbose, tracker, is_first_agent)

    # Original GitHub download logic
    if tracker:
        tracker.start(f"fetch-{ai_assistant}", "contacting GitHub API")
    try:
        zip_path, meta = download_template_from_github(
            ai_assistant,
            current_dir,
            script_type=script_type,
            verbose=verbose and tracker is None,
            show_progress=(tracker is None),
            client=client,
            debug=debug,
            github_token=github_token
        )
        if tracker:
            tracker.complete(f"fetch-{ai_assistant}", f"release {meta['release']} ({meta['size']:,} bytes)")
            tracker.add(f"download-{ai_assistant}", "Download template")
            tracker.complete(f"download-{ai_assistant}", meta['filename'])
    except Exception as e:
        if tracker:
            tracker.error(f"fetch-{ai_assistant}", str(e))
        else:
            if verbose:
                console.print(f"[red]Error downloading template:[/red] {e}")
        raise

    if tracker:
        tracker.add(f"extract-{ai_assistant}", "Extract template")
        tracker.start(f"extract-{ai_assistant}")
    elif verbose:
        console.print("Extracting template...")

    try:
        if not is_current_dir:
            project_path.mkdir(parents=True, exist_ok=True)

        with zipfile.ZipFile(zip_path, 'r') as zip_ref:
            zip_contents = zip_ref.namelist()
            if tracker:
                tracker.start("zip-list")
                tracker.complete("zip-list", f"{len(zip_contents)} entries")
            elif verbose:
                console.print(f"[cyan]ZIP contains {len(zip_contents)} items[/cyan]")

            if is_current_dir:
                with tempfile.TemporaryDirectory() as temp_dir:
                    temp_path = Path(temp_dir)
                    zip_ref.extractall(temp_path)

                    extracted_items = list(temp_path.iterdir())
                    if tracker:
                        tracker.start("extracted-summary")
                        tracker.complete("extracted-summary", f"temp {len(extracted_items)} items")
                    elif verbose:
                        console.print(f"[cyan]Extracted {len(extracted_items)} items to temp location[/cyan]")

                    source_dir = temp_path
                    if len(extracted_items) == 1 and extracted_items[0].is_dir():
                        source_dir = extracted_items[0]
                        if tracker:
                            tracker.add("flatten", "Flatten nested directory")
                            tracker.complete("flatten")
                        elif verbose:
                            console.print(f"[cyan]Found nested directory structure[/cyan]")

                    for item in source_dir.iterdir():
                        # Skip .rainbow folder for non-first agents to avoid redundancy
                        if not is_first_agent and item.name == ".rainbow":
                            continue

                        dest_path = project_path / item.name
                        if item.is_dir():
                            if dest_path.exists():
                                if verbose and not tracker:
                                    console.print(f"[yellow]Merging directory:[/yellow] {item.name}")
                                for sub_item in item.rglob('*'):
                                    if sub_item.is_file():
                                        rel_path = sub_item.relative_to(item)
                                        dest_file = dest_path / rel_path
                                        dest_file.parent.mkdir(parents=True, exist_ok=True)
                                        # Special handling for .vscode/settings.json - merge instead of overwrite
                                        if dest_file.name == "settings.json" and dest_file.parent.name == ".vscode":
                                            handle_vscode_settings(sub_item, dest_file, rel_path, verbose, tracker)
                                        else:
                                            shutil.copy2(sub_item, dest_file)
                            else:
                                shutil.copytree(item, dest_path)
                        else:
                            if dest_path.exists() and verbose and not tracker:
                                console.print(f"[yellow]Overwriting file:[/yellow] {item.name}")
                            shutil.copy2(item, dest_path)
                    if verbose and not tracker:
                        console.print(f"[cyan]Template files merged into current directory[/cyan]")
            else:
                # Extract to temp directory first so we can selectively copy
                with tempfile.TemporaryDirectory() as temp_dir:
                    temp_path = Path(temp_dir)
                    zip_ref.extractall(temp_path)

                    extracted_items = list(temp_path.iterdir())
                    if tracker:
                        tracker.start("extracted-summary")
                        tracker.complete("extracted-summary", f"temp {len(extracted_items)} items")
                    elif verbose:
                        console.print(f"[cyan]Extracted {len(extracted_items)} items to temp:[/cyan]")

                    # Handle nested directory structure
                    source_dir = temp_path
                    if len(extracted_items) == 1 and extracted_items[0].is_dir():
                        source_dir = extracted_items[0]
                        if tracker:
                            tracker.add("flatten", "Flatten nested directory")
                            tracker.complete("flatten")
                        elif verbose:
                            console.print(f"[cyan]Found nested directory structure[/cyan]")

                    # Copy items from temp to project_path, skipping .rainbow for non-first agents
                    for item in source_dir.iterdir():
                        # Skip .rainbow folder for non-first agents to avoid redundancy
                        if not is_first_agent and item.name == ".rainbow":
                            continue

                        dest_path = project_path / item.name
                        if item.is_dir():
                            shutil.copytree(item, dest_path, dirs_exist_ok=True)
                        else:
                            shutil.copy2(item, dest_path)

                    if verbose and not tracker:
                        console.print(f"[cyan]Template files copied to {project_path}[/cyan]")

    except Exception as e:
        if tracker:
            tracker.error(f"extract-{ai_assistant}", str(e))
        else:
            if verbose:
                console.print(f"[red]Error extracting template:[/red] {e}")
                if debug:
                    from rich.panel import Panel
                    console.print(Panel(str(e), title="Extraction Error", border_style="red"))

        if not is_current_dir and project_path.exists():
            shutil.rmtree(project_path)
        # Re-raise the original exception instead of typer.Exit to preserve error details
        raise
    else:
        if tracker:
            tracker.complete(f"extract-{ai_assistant}")
    finally:
        if tracker:
            tracker.add("cleanup", "Remove temporary archive")

        if zip_path.exists():
            zip_path.unlink()
            if tracker:
                tracker.complete("cleanup")
            elif verbose:
                console.print(f"Cleaned up: {zip_path.name}")

    return project_path
