---
description: Create or update the project ground-rules from interactive or provided principle inputs, ensuring all dependent templates stay in sync.
handoffs: 
  - label: Build Specification
    agent: rainbow.specify
    prompt: Create the feature specification based on the updated ground-rules. I want to build...
---

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Outline

You are creating or updating the project ground rules at `memory/ground-rules.md` based on the template at `.rainbow/templates/templates-for-commands/ground-rules-template.md`. The template contains placeholder tokens in square brackets (e.g. `[PROJECT_NAME]`, `[PRINCIPLE_1_NAME]`). Your job is to (a) copy the template, (b) collect/derive concrete values, (c) fill the template precisely, and (d) propagate any amendments across dependent artifacts. Incorporate the essential rules for maximizing source code readability, simplicity and long-term maintainability.

Follow this execution flow:

**IMPORTANT**: Automatically generate a 'docs:' prefixed git commit message (e.g., 'docs: establish project ground rules v1.0.0' or 'docs: update ground rules to v1.1.0') and commit memory/ground-rules.md and any updated template files upon completion.

1. Load the ground rules template from `.rainbow/templates/templates-for-commands/ground-rules-template.md`.
   - If `memory/ground-rules.md` already exists, read it to extract current values and version information.
   - Identify every placeholder token of the form `[ALL_CAPS_IDENTIFIER]` from the template.
   **IMPORTANT**: The user might require less or more principles than the ones used in the template. If a number is specified, respect that - follow the general template structure. You will update the doc accordingly.

2. Collect/derive values for placeholders:
   - If user input (conversation) supplies a value, use it.
   - If `memory/ground-rules.md` exists, extract current values from it (preserve existing principles unless user requests changes).
   - Otherwise infer from existing repo context (README, docs, project structure).
   - For governance dates:
     - `RATIFICATION_DATE` is the original adoption date (if unknown and this is first creation, use today; if updating existing ground-rules, preserve original date).
     - `LAST_AMENDED_DATE` is today if changes are made, otherwise keep previous date.
   - `CONSTITUTION_VERSION` must increment according to semantic versioning rules:
     - MAJOR: Backward incompatible governance/principle removals or redefinitions.
     - MINOR: New principle/section added or materially expanded guidance.
     - PATCH: Clarifications, wording, typo fixes, non-semantic refinements.
   - If this is the first ground rules document (no existing `memory/ground-rules.md`), start with version 1.0.0.
   - If version bump type ambiguous, propose reasoning before finalizing.

3. Draft the updated ground-rules content (based on the template):
   - Start with the template from `.rainbow/templates/templates-for-commands/ground-rules-template.md`.
   - Replace every placeholder with concrete text (no bracketed tokens left except intentionally retained template slots that the project has chosen not to define yet—explicitly justify any left).
   - Preserve heading hierarchy and comments can be removed once replaced unless they still add clarifying guidance.
   - Ensure each Principle section: succinct name line, paragraph (or bullet list) capturing non‑negotiable rules, explicit rationale if not obvious.
   - Ensure Governance section lists amendment procedure, versioning policy, and compliance review expectations.

4. Consistency propagation checklist (convert prior checklist into active validations):
   - Read `.rainbow/templates/templates-for-commands/design-template.md` and ensure any "Ground-rules Check" or rules align with updated principles.
   - Read `.rainbow/templates/templates-for-commands/spec-template.md` for scope/requirements alignment—update if ground-rules adds/removes mandatory sections or constraints.
   - Read `.rainbow/templates/templates-for-commands/tasks-template.md` and ensure task categorization reflects new or removed principle-driven task types (e.g., observability, versioning, testing discipline).
   - Read each command file in `commands/*.md` (including this one) to verify no outdated references remain when generic guidance is required.
   - Read any runtime guidance docs (e.g., `README.md`, `docs/quickstart.md`, or agent-specific guidance files if present). Update references to principles changed.

5. Produce a Sync Impact Report (prepend as an HTML comment at top of the ground-rules file after update):
   - Version change: old → new
   - List of modified principles (old title → new title if renamed)
   - Added sections
   - Removed sections
   - Templates requiring updates (✅ updated / ⚠ pending) with file paths
   - Follow-up TODOs if any placeholders intentionally deferred.

6. Validation before final output:
   - No remaining unexplained bracket tokens.
   - Version line matches report.
   - Dates ISO format YYYY-MM-DD.
   - Principles are declarative, testable, and free of vague language ("should" → replace with MUST/SHOULD rationale where appropriate).

7. Write the completed ground rules to `memory/ground-rules.md` (overwrite if exists, create if new).

8. Output a final summary to the user with:
   - New version and bump rationale (or "1.0.0 - Initial ground-rules" if first creation).
   - Any files flagged for manual follow-up.
   - Suggested commit message (e.g., `docs: amend ground-rules to vX.Y.Z (principle additions + governance update)` or `docs: create initial ground-rules v1.0.0`).

Formatting & Style Requirements:

- Use Markdown headings exactly as in the template (do not demote/promote levels).
- Wrap long rationale lines to keep readability (<100 chars ideally) but do not hard enforce with awkward breaks.
- Keep a single blank line between sections.
- Avoid trailing whitespace.

If the user supplies partial updates (e.g., only one principle revision), still perform validation and version decision steps.

If critical info missing (e.g., project name truly unknown on first creation), insert `TODO(<FIELD_NAME>): explanation` and include in the Sync Impact Report under deferred items.

The workflow: **Read template from `.rainbow/templates/templates-for-commands/ground-rules-template.md`** → **Copy and fill to `memory/ground-rules.md`** → **Validate and propagate changes to other templates**.

Do not modify the source template at `.rainbow/templates/templates-for-commands/ground-rules-template.md`; always work with the destination file at `memory/ground-rules.md`.
