# Writing Guidelines

Comprehensive guidelines for clear, consistent, and effective technical writing.

## Table of Contents

- [Clarity and Conciseness](#clarity-and-conciseness)
- [Consistent Terminology](#consistent-terminology)
- [Active Voice](#active-voice)
- [Specificity](#specificity)
- [Heading Hierarchy](#heading-hierarchy)
- [Code Formatting](#code-formatting)
- [Lists and Tables](#lists-and-tables)
- [Common Mistakes](#common-mistakes)

---

## Clarity and Conciseness

### Use Active Voice

Active voice makes documentation clearer and more direct.

**✅ Good Examples:**
- "The system processes the request"
- "Click Save to store your changes"
- "The API returns a JSON response"
- "Configure the settings in the dashboard"

**❌ Bad Examples:**
- "The request is processed by the system"
- "Your changes can be stored by clicking on the Save button"
- "A JSON response is returned by the API"
- "The settings can be configured in the dashboard"

### Be Direct and Concise

Eliminate unnecessary words and get to the point.

**✅ Good Examples:**
- "Configure the settings"
- "Run the command"
- "The function returns a boolean"
- "Authentication requires an API key"

**❌ Bad Examples:**
- "Configure the settings and parameters"
- "You should go ahead and run the command"
- "The function will go ahead and return a boolean value"
- "In order to authenticate, you will need to provide an API key"

### Avoid Redundancy

Don't repeat information unnecessarily.

**✅ Good Examples:**
- "Create a new user"
- "Delete the file"
- "Important: Backup data before upgrading"

**❌ Bad Examples:**
- "Create a new user account" (account is redundant)
- "Delete and remove the file" (delete and remove mean the same)
- "Important note: Please note that..." (redundant "note")

### Define Acronyms and Jargon

Always define acronyms on first use and explain technical terms.

**✅ Good Examples:**
- "API (Application Programming Interface)"
- "JWT (JSON Web Token) for authentication"
- "Use CRUD (Create, Read, Update, Delete) operations"
- "Configure the REST (Representational State Transfer) endpoint"

**❌ Bad Examples:**
- "Configure the API" (assumes everyone knows API)
- "Use JWT for auth" (unexplained acronyms)
- "Implement CRUD" (no explanation)

### Use Short Sentences

Keep sentences concise and focused on one idea.

**✅ Good Example:**
```markdown
The API supports pagination. Use the `page` parameter to specify the page number. Each page returns up to 100 results.
```

**❌ Bad Example:**
```markdown
The API supports pagination and you can use the `page` parameter to specify which page number you want to retrieve, with each page returning up to 100 results depending on your configuration.
```

---

## Consistent Terminology

### Choose One Term and Stick With It

Don't alternate between synonyms for the same concept.

**✅ Good (Consistent):**
```markdown
## User Management

To create a user, click "Add User".
To edit a user, select the user from the list.
To delete a user, click the trash icon next to the user.
```

**❌ Bad (Inconsistent):**
```markdown
## User Management

To create a customer, click "Add User".
To edit a client, select the account from the list.
To delete a person, click the trash icon next to the member.
```

### Create and Use a Glossary

Document your terminology and share with the team.

**Example Glossary:**
```markdown
# Terminology

- **User**: Person who interacts with the system (not customer, client, or account)
- **Project**: Collection of tasks and resources (not workspace or folder)
- **API Key**: Authentication credential (not access token or secret key)
- **Endpoint**: API route (not URL, path, or resource)
```

### Be Consistent with Technical Terms

Use standard industry terminology correctly.

**✅ Correct:**
- REST API (not RESTful API or REST endpoint)
- JSON (all caps, not Json or json)
- JavaScript (not Javascript or java script)
- macOS (not MacOS or Mac OS)
- URL (not Url or url)
- HTTP (not Http or http)

---

## Active Voice

### Prefer Active Over Passive

Active voice is more engaging and clearer.

**✅ Active Voice:**
- "The API validates the input"
- "Run the migration script"
- "The user clicks the button"
- "Configure the database connection"

**❌ Passive Voice:**
- "The input is validated by the API"
- "The migration script should be run"
- "The button is clicked by the user"
- "The database connection should be configured"

### When Passive Voice is Acceptable

Use passive voice when the actor is unknown or unimportant.

**Acceptable Examples:**
- "The user was created successfully" (focus on the result)
- "The file has been deleted" (who deleted it doesn't matter)
- "An error was encountered" (source of error is unknown)

---

## Specificity

### Be Specific with Numbers

Provide exact numbers instead of vague terms.

**✅ Specific:**
- "Response time < 200ms for 95% of requests"
- "Retry up to 3 times with 2-second delays"
- "Password must be 12+ characters"
- "API rate limit: 1000 requests per hour"
- "Cache expires after 5 minutes"

**❌ Vague:**
- "Fast response time"
- "Retry a few times"
- "Strong password required"
- "High API rate limit"
- "Short cache duration"

### Be Specific with Instructions

Provide exact steps, not general guidance.

**✅ Specific:**
```markdown
1. Open terminal
2. Navigate to project directory: `cd /path/to/project`
3. Run: `npm install`
4. Start server: `npm start`
5. Open browser to: http://localhost:3000
```

**❌ Vague:**
```markdown
1. Open your terminal
2. Go to the project folder
3. Install the dependencies
4. Start the server
5. Check if it's working
```

### Specify Versions and Requirements

Always include version numbers and system requirements.

**✅ Specific:**
- "Requires Node.js 18.0 or higher"
- "Compatible with Python 3.9, 3.10, 3.11"
- "Tested on Ubuntu 22.04 LTS"
- "Supports PostgreSQL 14+ and MySQL 8+"

**❌ Vague:**
- "Requires recent Node.js"
- "Works with Python 3.x"
- "Compatible with modern Linux"
- "Supports major databases"

---

## Heading Hierarchy

### Use Proper Heading Levels

Follow semantic heading structure (H1 → H2 → H3).

**✅ Correct Hierarchy:**
```markdown
# Main Title (H1)

## Major Section (H2)

### Subsection (H3)

#### Minor Point (H4)

## Another Major Section (H2)

### Subsection (H3)
```

**❌ Incorrect (Skipping Levels):**
```markdown
# Main Title (H1)

### Subsection (H3) - skipped H2

##### Minor Point (H5) - skipped H4
```

### Make Headings Descriptive

Headings should clearly indicate section content.

**✅ Descriptive:**
- "Installing Prerequisites"
- "Configuring Authentication"
- "Troubleshooting Connection Errors"
- "API Rate Limits"

**❌ Vague:**
- "Setup"
- "Configuration"
- "Problems"
- "Limits"

---

## Code Formatting

### Always Specify Language

Always include language identifier in code blocks.

**✅ With Language:**
````markdown
```javascript
const result = await fetchData();
console.log(result);
```
````

**❌ Without Language:**
````markdown
```
const result = await fetchData();
console.log(result);
```
````

### Use Inline Code for Keywords

Wrap code elements in backticks.

**✅ Good:**
- Use the `GET` method to retrieve data
- Set the `Content-Type` header to `application/json`
- The `userId` parameter is required
- Run `npm install` to install dependencies

**❌ Bad:**
- Use the GET method to retrieve data
- Set the Content-Type header to application/json
- The userId parameter is required
- Run npm install to install dependencies

### Format Code Examples Properly

Show complete, runnable code with proper formatting.

**✅ Complete Example:**
```javascript
// Fetch user data
async function getUserById(id) {
  try {
    const response = await fetch(`/api/users/${id}`);
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}`);
    }
    const user = await response.json();
    return user;
  } catch (error) {
    console.error('Failed to fetch user:', error);
    throw error;
  }
}
```

**❌ Incomplete Example:**
```javascript
// Get user
fetch('/api/users/' + id)
  .then(res => res.json())
```

### Include Input/Output Pairs

Show both input and expected output.

**✅ With Input/Output:**
```javascript
// Input
const input = "hello world";
const result = capitalizeWords(input);
console.log(result);

// Output
// "Hello World"
```

---

## Lists and Tables

### Use Lists for Sequential Steps

Number lists for steps, bullets for items.

**✅ Numbered for Steps:**
```markdown
## Installation

1. Install Node.js from https://nodejs.org
2. Clone the repository: `git clone ...`
3. Install dependencies: `npm install`
4. Start the server: `npm start`
```

**✅ Bullets for Non-Sequential Items:**
```markdown
## Features

- User authentication
- Real-time updates
- File upload support
- Email notifications
```

### Use Tables for Structured Data

Tables are ideal for parameters, options, and comparisons.

**✅ Good Table:**
```markdown
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| name | string | Yes | User's full name |
| email | string | Yes | Valid email address |
| age | integer | No | User's age (18+) |
```

**❌ Poor Formatting:**
```markdown
Parameters:
- name (string, required): User's full name
- email (string, required): Valid email address
- age (integer, optional): User's age (must be 18 or older)
```

---

## Common Mistakes

### Don't Use "You Should" or "You Might Want To"

Be direct with imperative mood.

**✅ Direct:**
- "Install Node.js 18 or higher"
- "Configure the database URL"
- "Run tests before deploying"

**❌ Wishy-Washy:**
- "You should install Node.js 18 or higher"
- "You might want to configure the database URL"
- "You should probably run tests before deploying"

### Don't Use Future Tense for Documentation

Use present tense for current functionality.

**✅ Present Tense:**
- "The API returns a JSON response"
- "The function validates user input"
- "Authentication requires an API key"

**❌ Future Tense:**
- "The API will return a JSON response"
- "The function will validate user input"
- "Authentication will require an API key"

### Don't Assume Gender

Use gender-neutral language.

**✅ Gender-Neutral:**
- "The user enters their email"
- "Each developer has their own API key"
- "The admin can configure their preferences"

**❌ Gendered:**
- "The user enters his email"
- "Each developer has his own API key"
- "The admin can configure her preferences"

### Don't Use Contractions

Use full words in formal documentation.

**✅ Full Words:**
- "do not"
- "cannot"
- "it is"
- "you are"

**❌ Contractions:**
- "don't"
- "can't"
- "it's"
- "you're"

### Avoid "Just" and "Simply"

These words can be condescending or dismissive.

**✅ Without "Just":**
- "Run `npm install` to install dependencies"
- "Add the API key to your environment variables"
- "Configure the database connection string"

**❌ With "Just":**
- "Just run `npm install` to install dependencies"
- "Simply add the API key to your environment variables"
- "Just configure the database connection string"

---

## Formatting Standards

### Capitalization

**Sentence Case for Headings:**
- ✅ "Getting started with the API"
- ❌ "Getting Started With The API"

**Exception - Title Case for Main Titles:**
- ✅ "API Reference Guide"
- ✅ "User Authentication Tutorial"

**Proper Nouns Always Capitalized:**
- JavaScript, Python, PostgreSQL, Docker, AWS

### Punctuation

**End Sentences with Periods:**
```markdown
✅ Configure the database. Run migrations. Start the server.
❌ Configure the database
```

**No Period for Headings:**
```markdown
✅ ## Installing Dependencies
❌ ## Installing Dependencies.
```

**Use Periods in Lists (for Complete Sentences):**
```markdown
✅ 
- Install Node.js 18 or higher.
- Clone the repository from GitHub.
- Run `npm install` to install dependencies.

❌ 
- Install Node.js 18 or higher
- Clone the repository from GitHub
- Run `npm install` to install dependencies
```

### Line Length

**Keep Lines Reasonable:**
- Target: 80-100 characters per line in markdown
- Helps with readability and diffs
- Exception: URLs and code blocks can be longer

---

## Accessibility

### Provide Alt Text for Images

Always include descriptive alt text.

**✅ With Alt Text:**
```markdown
![Login screen showing username and password fields](images/login-screen.png)
```

**❌ Without Alt Text:**
```markdown
![](images/login-screen.png)
```

### Use Descriptive Link Text

Link text should describe the destination.

**✅ Descriptive:**
- See the [API authentication guide](link) for details
- Download the [installation script](link)
- View [complete code example](link)

**❌ Non-Descriptive:**
- See [here](link) for details
- Click [this link](link)
- View [example](link)

### Ensure Proper Heading Hierarchy

Screen readers rely on heading hierarchy for navigation.

**✅ Proper Hierarchy:**
- Never skip heading levels (H1 → H2 → H3)
- Each page has exactly one H1
- Headings are descriptive

---

## Review Checklist

Before publishing documentation:

- [ ] All acronyms defined on first use
- [ ] Active voice used throughout
- [ ] Specific numbers and versions provided
- [ ] Code blocks have language specified
- [ ] Consistent terminology used
- [ ] Proper heading hierarchy (no skipped levels)
- [ ] Tables formatted correctly
- [ ] Links are descriptive
- [ ] Images have alt text
- [ ] No gendered language
- [ ] No contractions in formal docs
- [ ] Grammar and spelling checked
