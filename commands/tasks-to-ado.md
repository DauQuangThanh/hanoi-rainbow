---
description: Convert existing tasks into actionable, dependency-ordered Azure DevOps work items for the feature based on available design artifacts.
tools: ['ado/azure-devops-mcp/mcp_ado_wit_create_work_item']
scripts:
  sh: scripts/bash/check-prerequisites.sh --json --require-tasks --include-tasks
  ps: scripts/powershell/check-prerequisites.ps1 -Json -RequireTasks -IncludeTasks
---

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Outline

**IMPORTANT**: After creating all Azure DevOps work items, automatically generate a 'chore:' prefixed git commit message (e.g., 'chore: sync tasks to Azure DevOps work items') and commit any tracking files or metadata upon completion.

1. Run `{SCRIPT}` from repo root and parse FEATURE_DIR and AVAILABLE_DOCS list. All paths must be absolute. For single quotes in args like "I'm Groot", use escape syntax: e.g 'I'\''m Groot' (or double-quote if possible: "I'm Groot").
1. From the executed script, extract the path to **tasks**.
1. Get the Git remote by running:

```bash
git config --get remote.origin.url
```

**ONLY PROCEED TO NEXT STEPS IF THE REMOTE IS AN AZURE DEVOPS URL**

Azure DevOps remotes can be in these formats:

- `https://dev.azure.com/{organization}/{project}/_git/{repository}`
- `https://{organization}.visualstudio.com/{project}/_git/{repository}`
- `https://{organization}.visualstudio.com/DefaultCollection/{project}/_git/{repository}`
- `git@ssh.dev.azure.com:v3/{organization}/{project}/{repository}`

1. Extract the Azure DevOps organization and project from the remote URL based on the pattern detected.

1. For each task in the list, use the Azure DevOps MCP server to create a new work item in the project that matches the Git remote.

**UNDER NO CIRCUMSTANCES EVER CREATE WORK ITEMS IN PROJECTS THAT DO NOT MATCH THE REMOTE URL**
