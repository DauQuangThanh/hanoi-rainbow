#!/usr/bin/env bash
set -euo pipefail

# create-release-packages.sh (workflow-local)
# Build Hanoi Rainbow template release archives for each supported AI assistant and script type.
# Usage: .github/workflows/scripts/create-release-packages.sh <version>
#   Version argument should include leading 'v'.
#   Optionally set AGENTS and/or SCRIPTS env vars to limit what gets built.
#     AGENTS  : space or comma separated subset of: claude gemini copilot cursor-agent qwen opencode windsurf codex amp shai bob (default: all)
#     SCRIPTS : space or comma separated subset of: sh ps (default: both)
#   Examples:
#     AGENTS=claude SCRIPTS=sh $0 v0.2.0
#     AGENTS="copilot,gemini" $0 v0.2.0
#     SCRIPTS=ps $0 v0.2.0

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <version-with-v-prefix>" >&2
  exit 1
fi
NEW_VERSION="$1"
if [[ ! $NEW_VERSION =~ ^v[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "Version must look like v0.0.0" >&2
  exit 1
fi

echo "Building release packages for $NEW_VERSION"

# Create and use .genreleases directory for all build artifacts
GENRELEASES_DIR=".genreleases"
mkdir -p "$GENRELEASES_DIR"
rm -rf "$GENRELEASES_DIR"/* || true

rewrite_paths() {
  sed -E \
    -e 's@(/?)memory/@memory/@g' \
    -e 's@(/?)scripts/@.rainbow/scripts/@g' \
    -e 's@(/?)templates/@.rainbow/templates/@g'
}

generate_commands() {
  local agent=$1 ext=$2 arg_format=$3 output_dir=$4 script_variant=$5
  mkdir -p "$output_dir"
  for template in commands/*.md; do
    [[ -f "$template" ]] || continue
    local name description script_command agent_script_command body
    name=$(basename "$template" .md)
    
    # Normalize line endings
    file_content=$(tr -d '\r' < "$template")
    
    # Extract description and script command from YAML frontmatter
    description=$(printf '%s\n' "$file_content" | awk '/^description:/ {sub(/^description:[[:space:]]*/, ""); print; exit}')
    script_command=$(printf '%s\n' "$file_content" | awk -v sv="$script_variant" '/^[[:space:]]*'"$script_variant"':[[:space:]]*/ {sub(/^[[:space:]]*'"$script_variant"':[[:space:]]*/, ""); print; exit}')
    
    if [[ -z $script_command ]]; then
      echo "Warning: no script command found for $script_variant in $template" >&2
      script_command="(Missing script command for $script_variant)"
    fi
    
    # Extract agent_script command from YAML frontmatter if present
    agent_script_command=$(printf '%s\n' "$file_content" | awk '
      /^agent_scripts:$/ { in_agent_scripts=1; next }
      in_agent_scripts && /^[[:space:]]*'"$script_variant"':[[:space:]]*/ {
        sub(/^[[:space:]]*'"$script_variant"':[[:space:]]*/, "")
        print
        exit
      }
      in_agent_scripts && /^[a-zA-Z]/ { in_agent_scripts=0 }
    ')
    
    # Replace {SCRIPT} placeholder with the script command
    body=$(printf '%s\n' "$file_content" | sed "s|{SCRIPT}|${script_command}|g")
    
    # Replace {AGENT_SCRIPT} placeholder with the agent script command if found
    if [[ -n $agent_script_command ]]; then
      body=$(printf '%s\n' "$body" | sed "s|{AGENT_SCRIPT}|${agent_script_command}|g")
    fi
    
    # Remove the scripts: and agent_scripts: sections from frontmatter while preserving YAML structure
    body=$(printf '%s\n' "$body" | awk '
      /^---$/ { print; if (++dash_count == 1) in_frontmatter=1; else in_frontmatter=0; next }
      in_frontmatter && /^scripts:$/ { skip_scripts=1; next }
      in_frontmatter && /^agent_scripts:$/ { skip_scripts=1; next }
      in_frontmatter && /^[a-zA-Z].*:/ && skip_scripts { skip_scripts=0 }
      in_frontmatter && skip_scripts && /^[[:space:]]/ { next }
      { print }
    ')
    
    # Apply other substitutions
    body=$(printf '%s\n' "$body" | sed "s/{ARGS}/$arg_format/g" | sed "s/__AGENT__/$agent/g" | rewrite_paths)
    
    case $ext in
      toml)
        body=$(printf '%s\n' "$body" | sed 's/\\/\\\\/g')
        { echo "description = \"$description\""; echo; echo "prompt = \"\"\""; echo "$body"; echo "\"\"\""; } > "$output_dir/rainbow.$name.$ext" ;;
      md)
        echo "$body" > "$output_dir/rainbow.$name.$ext" ;;
      agent.md)
        echo "$body" > "$output_dir/rainbow.$name.$ext" ;;
    esac
  done
}

generate_skills() {
  local agent=$1 output_dir=$2
  [[ -d skills ]] || return 0
  mkdir -p "$output_dir"
  
  # Copy all skill subdirectories
  for skill_dir in skills/*; do
    [[ -d "$skill_dir" ]] || continue
    local skill_name=$(basename "$skill_dir")
    cp -r "$skill_dir" "$output_dir/$skill_name"
  done
  
  echo "Copied skills -> $output_dir"
}

build_variant() {
  local agent=$1 script=$2
  local base_dir="$GENRELEASES_DIR/sdd-${agent}-package-${script}"
  echo "Building $agent ($script) package..."
  mkdir -p "$base_dir"
  
  # Copy base structure but filter scripts by variant
  SPEC_DIR="$base_dir/.rainbow"
  mkdir -p "$SPEC_DIR"
  
  [[ -d memory ]] && { cp -r memory "$SPEC_DIR/"; echo "Copied memory -> .rainbow"; }
  
  # Only copy the relevant script variant directory
  if [[ -d scripts ]]; then
    mkdir -p "$SPEC_DIR/scripts"
    case $script in
      sh)
        [[ -d scripts/bash ]] && { cp -r scripts/bash "$SPEC_DIR/scripts/"; echo "Copied scripts/bash -> .rainbow/scripts"; }
        # Copy any script files that aren't in variant-specific directories
        find scripts -maxdepth 1 -type f -exec cp {} "$SPEC_DIR/scripts/" \; 2>/dev/null || true
        ;;
      ps)
        [[ -d scripts/powershell ]] && { cp -r scripts/powershell "$SPEC_DIR/scripts/"; echo "Copied scripts/powershell -> .rainbow/scripts"; }
        # Copy any script files that aren't in variant-specific directories
        find scripts -maxdepth 1 -type f -exec cp {} "$SPEC_DIR/scripts/" \; 2>/dev/null || true
        ;;
    esac
  fi
  
  # Copy template files to .rainbow/templates with subdirectories
  mkdir -p "$SPEC_DIR/templates"
  [[ -d commands/templates-for-commands ]] && {
    mkdir -p "$SPEC_DIR/templates/templates-for-commands"
    find commands/templates-for-commands -type f -not -name "vscode-settings.json" -exec cp {} "$SPEC_DIR/templates/templates-for-commands/" \;
    echo "Copied commands/templates-for-commands -> .rainbow/templates/templates-for-commands"
  }
  
  # NOTE: We substitute {ARGS} internally. Outward tokens differ intentionally:
  #   * Markdown/prompt (claude, copilot, cursor-agent, opencode): $ARGUMENTS
  #   * TOML (gemini, qwen): {{args}}
  # This keeps formats readable without extra abstraction.

  case $agent in
    claude)
      mkdir -p "$base_dir/.claude/commands"
      generate_commands claude md "\$ARGUMENTS" "$base_dir/.claude/commands" "$script"
      generate_skills claude "$base_dir/.claude/skills" ;;
    gemini)
      mkdir -p "$base_dir/.gemini/commands"
      generate_commands gemini toml "{{args}}" "$base_dir/.gemini/commands" "$script"
      generate_skills gemini "$base_dir/.gemini/extensions"
      [[ -f agent_templates/gemini/GEMINI.md ]] && cp agent_templates/gemini/GEMINI.md "$base_dir/GEMINI.md" ;;
    copilot)
      mkdir -p "$base_dir/.github/agents"
      mkdir -p "$base_dir/.github/prompts"
      generate_commands copilot agent.md "\$ARGUMENTS" "$base_dir/.github/agents" "$script"
      # Also generate prompts for slash commands
      for template in commands/*.md; do
        [[ -f "$template" ]] || continue
        local name file_content description script_command agent_script_command body
        name=$(basename "$template" .md)
        file_content=$(tr -d '\r' < "$template")
        description=$(printf '%s\n' "$file_content" | awk '/^description:/ {sub(/^description:[[:space:]]*/, ""); print; exit}')
        script_command=$(printf '%s\n' "$file_content" | awk -v sv="$script" '/^[[:space:]]*'"$script"':[[:space:]]*/ {sub(/^[[:space:]]*'"$script"':[[:space:]]*/, ""); print; exit}')
        [[ -z $script_command ]] && script_command="(Missing script command for $script)"
        agent_script_command=$(printf '%s\n' "$file_content" | awk '
          /^agent_scripts:$/ { in_agent_scripts=1; next }
          in_agent_scripts && /^[[:space:]]*'"$script"':[[:space:]]*/ {
            sub(/^[[:space:]]*'"$script"':[[:space:]]*/, "")
            print
            exit
          }
          in_agent_scripts && /^[a-zA-Z]/ { in_agent_scripts=0 }
        ')
        body=$(printf '%s\n' "$file_content" | sed "s|{SCRIPT}|${script_command}|g")
        [[ -n $agent_script_command ]] && body=$(printf '%s\n' "$body" | sed "s|{AGENT_SCRIPT}|${agent_script_command}|g")
        body=$(printf '%s\n' "$body" | awk '
          /^---$/ { print; if (++dash_count == 1) in_frontmatter=1; else in_frontmatter=0; next }
          in_frontmatter && /^scripts:$/ { skip_scripts=1; next }
          in_frontmatter && /^agent_scripts:$/ { skip_scripts=1; next }
          in_frontmatter && /^[a-zA-Z].*:/ && skip_scripts { skip_scripts=0 }
          in_frontmatter && skip_scripts && /^[[:space:]]/ { next }
          { print }
        ')
        body=$(printf '%s\n' "$body" | sed "s/{ARGS}/\$ARGUMENTS/g" | sed "s/__AGENT__/copilot/g" | rewrite_paths)
        echo "$body" > "$base_dir/.github/prompts/rainbow.$name.prompt.md"
      done
      generate_skills copilot "$base_dir/.github/skills"
      # Create VS Code workspace settings
      mkdir -p "$base_dir/.vscode"
      [[ -f commands/templates-for-commands/vscode-settings.json ]] && cp commands/templates-for-commands/vscode-settings.json "$base_dir/.vscode/settings.json"
      ;;
    cursor-agent)
      mkdir -p "$base_dir/.cursor/commands"
      generate_commands cursor-agent md "\$ARGUMENTS" "$base_dir/.cursor/commands" "$script"
      generate_skills cursor-agent "$base_dir/.cursor/rules" ;;
    qwen)
      mkdir -p "$base_dir/.qwen/commands"
      generate_commands qwen toml "{{args}}" "$base_dir/.qwen/commands" "$script"
      generate_skills qwen "$base_dir/.qwen/skills"
      [[ -f agent_templates/qwen/QWEN.md ]] && cp agent_templates/qwen/QWEN.md "$base_dir/QWEN.md" ;;
    opencode)
      mkdir -p "$base_dir/.opencode/command"
      generate_commands opencode md "\$ARGUMENTS" "$base_dir/.opencode/command" "$script"
      generate_skills opencode "$base_dir/.opencode/skill" ;;
    windsurf)
      mkdir -p "$base_dir/.windsurf/workflows"
      generate_commands windsurf md "\$ARGUMENTS" "$base_dir/.windsurf/workflows" "$script"
      generate_skills windsurf "$base_dir/.windsurf/skills" ;;
    codex)
      mkdir -p "$base_dir/.codex/commands"
      generate_commands codex md "\$ARGUMENTS" "$base_dir/.codex/commands" "$script"
      generate_skills codex "$base_dir/.codex/skills" ;;
    kilocode)
      mkdir -p "$base_dir/.kilocode/rules"
      generate_commands kilocode md "\$ARGUMENTS" "$base_dir/.kilocode/rules" "$script"
      generate_skills kilocode "$base_dir/.kilocode/skills" ;;
    auggie)
      mkdir -p "$base_dir/.augment/rules"
      generate_commands auggie md "\$ARGUMENTS" "$base_dir/.augment/rules" "$script"
      generate_skills auggie "$base_dir/.augment/rules" ;;
    roo)
      mkdir -p "$base_dir/.roo/rules"
      generate_commands roo md "\$ARGUMENTS" "$base_dir/.roo/rules" "$script"
      generate_skills roo "$base_dir/.roo/skills" ;;
    codebuddy)
      mkdir -p "$base_dir/.codebuddy/commands"
      generate_commands codebuddy md "\$ARGUMENTS" "$base_dir/.codebuddy/commands" "$script"
      generate_skills codebuddy "$base_dir/.codebuddy/skills" ;;
    amp)
      mkdir -p "$base_dir/.agents/commands"
      generate_commands amp md "\$ARGUMENTS" "$base_dir/.agents/commands" "$script"
      generate_skills amp "$base_dir/.agents/skills" ;;
    shai)
      mkdir -p "$base_dir/.shai/commands"
      generate_commands shai md "\$ARGUMENTS" "$base_dir/.shai/commands" "$script"
      generate_skills shai "$base_dir/.shai/commands" ;;
    q)
      mkdir -p "$base_dir/.amazonq/prompts"
      generate_commands q md "\$ARGUMENTS" "$base_dir/.amazonq/prompts" "$script"
      generate_skills q "$base_dir/.amazonq/cli-agents" ;;
    bob)
      mkdir -p "$base_dir/.bob/commands"
      generate_commands bob md "\$ARGUMENTS" "$base_dir/.bob/commands" "$script"
      generate_skills bob "$base_dir/.bob/skills" ;;
    jules)
      mkdir -p "$base_dir/.agent"
      generate_commands jules md "\$ARGUMENTS" "$base_dir/.agent" "$script"
      generate_skills jules "$base_dir/skills" ;;
    qoder)
      mkdir -p "$base_dir/.qoder/commands"
      generate_commands qoder md "\$ARGUMENTS" "$base_dir/.qoder/commands" "$script"
      generate_skills qoder "$base_dir/.qoder/skills" ;;
    antigravity)
      mkdir -p "$base_dir/.agent/rules"
      generate_commands antigravity md "\$ARGUMENTS" "$base_dir/.agent/rules" "$script"
      generate_skills antigravity "$base_dir/.agent/skills" ;;
  esac
  ( cd "$base_dir" && zip -r "../rainbow-template-${agent}-${script}-${NEW_VERSION}.zip" . )
  echo "Created $GENRELEASES_DIR/rainbow-template-${agent}-${script}-${NEW_VERSION}.zip"
}

# Determine agent list
ALL_AGENTS=(claude gemini copilot cursor-agent qwen opencode windsurf codex kilocode auggie roo codebuddy amp shai q bob jules qoder antigravity)
ALL_SCRIPTS=(sh ps)

norm_list() {
  # convert comma+space separated -> line separated unique while preserving order of first occurrence
  tr ',\n' '  ' | awk '{for(i=1;i<=NF;i++){if(!seen[$i]++){printf((out?"\n":"") $i);out=1}}}END{printf("\n")}'
}

validate_subset() {
  local type=$1; shift; local -n allowed=$1; shift; local items=("$@")
  local invalid=0
  for it in "${items[@]}"; do
    local found=0
    for a in "${allowed[@]}"; do [[ $it == "$a" ]] && { found=1; break; }; done
    if [[ $found -eq 0 ]]; then
      echo "Error: unknown $type '$it' (allowed: ${allowed[*]})" >&2
      invalid=1
    fi
  done
  return $invalid
}

if [[ -n ${AGENTS:-} ]]; then
  mapfile -t AGENT_LIST < <(printf '%s' "$AGENTS" | norm_list)
  validate_subset agent ALL_AGENTS "${AGENT_LIST[@]}" || exit 1
else
  AGENT_LIST=("${ALL_AGENTS[@]}")
fi

if [[ -n ${SCRIPTS:-} ]]; then
  mapfile -t SCRIPT_LIST < <(printf '%s' "$SCRIPTS" | norm_list)
  validate_subset script ALL_SCRIPTS "${SCRIPT_LIST[@]}" || exit 1
else
  SCRIPT_LIST=("${ALL_SCRIPTS[@]}")
fi

echo "Agents: ${AGENT_LIST[*]}"
echo "Scripts: ${SCRIPT_LIST[*]}"

for agent in "${AGENT_LIST[@]}"; do
  for script in "${SCRIPT_LIST[@]}"; do
    build_variant "$agent" "$script"
  done
done

echo "Archives in $GENRELEASES_DIR:"
ls -1 "$GENRELEASES_DIR"/rainbow-template-*-"${NEW_VERSION}".zip

