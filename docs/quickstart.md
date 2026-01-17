# ⚡ Quick Start Guide

**Build your first project with Spec-Driven Development in 8 steps.**

> **Note:** All scripts come in both Bash (`.sh`) and PowerShell (`.ps1`) versions. Rainbow CLI automatically picks the right one for your system unless you specify `--script sh|ps`.

---

## 🎯 The Workflow

Follow this order for best results:

> **💡 Automatic Version Control:** All Rainbow commands automatically generate appropriate git commit messages and commit changes upon completion using semantic commit prefixes (`docs:`, `feat:`, `test:`, `chore:`).

| Step | Command | Purpose |
|------|---------|----------|
| 1️⃣ | `/rainbow.regulate` | Set ground rules |
| 2️⃣ | `/rainbow.specify` | Define requirements |
| 3️⃣ | `/rainbow.architect` | Design system architecture (optional, once per product) |
| 4️⃣ | `/rainbow.standardize` | Create coding standards (optional, once per product) |
| 5️⃣ | `/rainbow.design` | Create implementation plan |
| 6️⃣ | `/rainbow.taskify` | Break down into tasks |
| 7️⃣ | `/rainbow.implement` | Build it! |

> **💡 Smart Context:** Rainbow automatically detects your active feature from your Git branch (like `001-feature-name`). To work on different features, just switch branches.

---

## 🎭 Role-Based Agents

---

## 🚀 Let's Build Something

### Step 1: Install Rainbow

Run this in your terminal:

```bash
# Create a new project
uvx --from git+https://github.com/dauquangthanh/hanoi-rainbow.git rainbow init <PROJECT_NAME>

# OR work in current directory
uvx --from git+https://github.com/dauquangthanh/hanoi-rainbow.git rainbow init .
```

**Want a specific script type?**

```bash
# Force PowerShell
uvx --from git+https://github.com/dauquangthanh/hanoi-rainbow.git rainbow init <PROJECT_NAME> --script ps

# Force Bash
uvx --from git+https://github.com/dauquangthanh/hanoi-rainbow.git rainbow init <PROJECT_NAME> --script sh
```

---

### Step 2: Set Your Rules

In your AI agent, use the `/rainbow.regulate` command to set project principles:

```bash
/rainbow.regulate This project follows a "Library-First" approach. All features must be implemented as standalone libraries first. We use TDD strictly. We prefer functional programming patterns.
```

**What this does:** Creates ground rules that guide all future development decisions.

---

### Step 3: Write Your Specification

Describe **what** you want (not **how** to build it):

```bash
/rainbow.specify Build a photo organizer app. Albums are grouped by date and can be reorganized by drag-and-drop. Each album shows photos in a tile view. No nested albums allowed.
```

**Focus on:** User needs, features, and behavior—skip tech stack details for now.

### Step 4: Design System Architecture *(Optional)*

Document your overall system design (do this once per product):

```bash
/rainbow.architect Document the system architecture including C4 diagrams, microservices design, and technology stack decisions.
```

---

### Step 5: Set Coding Standards *(Optional)*

Create team coding conventions (do this once per product):

```bash
/rainbow.standardize Create comprehensive coding standards for TypeScript and React, including naming conventions and best practices.
```

---

### Step 6: Refine Your Spec *(Optional)*

Clarify any unclear requirements:

```bash
/rainbow.clarify Focus on security and performance requirements.
```

---

### Step 7: Create Implementation Plan

Now specify **how** to build it (tech stack and architecture):

```bash
/rainbow.design Use Vite with minimal libraries. Stick to vanilla HTML, CSS, and JavaScript. Store metadata in local SQLite. No image uploads.
```

**What to include:** Tech stack, frameworks, libraries, database choices, architecture patterns.

---

### Step 8: Break Down & Build

**Create tasks:**

```bash
/rainbow.taskify
```

**Validate the plan (optional):**

```bash
/rainbow.analyze
```

**Build it:**

```bash
/rainbow.implement
```

**What happens:** Your AI agent executes all tasks in order, building your application according to the plan.

---

## 📖 Complete Example: Building Taskify

**Project:** A team productivity platform with Kanban boards.

### 1. Set Ground Rules

```bash
/rainbow.regulate Taskify is "Security-First". Validate all user inputs. Use microservices architecture. Document all code thoroughly.
```

### 2. Define Requirements

```bash
/rainbow.specify Build Taskify, a team productivity platform. Users can create projects, add team members, assign tasks, comment, and move tasks between Kanban boards. Start with 5 predefined users: 1 product manager and 4 engineers. Create 3 sample projects. Use standard Kanban columns: To Do, In Progress, In Review, Done. No login required for this initial version.
```

### 3. Refine with Details

```bash
/rainbow.clarify For task cards: users can change status by dragging between columns, leave unlimited comments, and assign tasks to any user. Show a user picker on launch. Clicking a user shows their projects. Clicking a project opens the Kanban board. Highlight tasks assigned to current user in different color. Users can edit/delete only their own comments.
```

### 4. Validate Specification

```bash
/rainbow.checklist
```

### 5. Create Technical Plan

```bash
/rainbow.design Use .NET Aspire with Postgres database. Frontend: Blazor server with drag-and-drop and real-time updates. Create REST APIs for projects, tasks, and notifications.
```

### 6. Validate and Build

```bash
/rainbow.analyze
/rainbow.implement
```

---

## 🎯 Key Principles

| Principle | What It Means |
|-----------|---------------|
| **Be Explicit** | Clearly describe what and why you're building |
| **Skip Tech Early** | Don't worry about tech stack during specification |
| **Iterate** | Refine specs before implementation |
| **Validate First** | Check the plan before coding |
| **Let AI Work** | Trust the agent to handle implementation details |

---

## 📚 Next Steps

**Learn more:**

- 📖 [Complete Methodology](../spec-driven.md) - Deep dive into the full process
- 🔍 [More Examples](../templates) - Explore sample projects
- 💻 [Source Code](https://github.com/dauquangthanh/hanoi-rainbow) - Contribute to the project

**Get help:**

- 🐛 [Report Issues](https://github.com/dauquangthanh/hanoi-rainbow/issues/new) - Found a bug?
- 💬 [Ask Questions](https://github.com/dauquangthanh/hanoi-rainbow/discussions) - Need help?
