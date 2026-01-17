# Review Workflow


### Step 1: Initial Assessment

**Gather Context:**
- Identify programming language and framework
- Understand project type (web app, API, library, CLI, etc.)
- Note any existing coding standards or style guides
- Check for configuration files (.eslintrc, .pylintrc, checkstyle.xml, etc.)

**Read the Code:**
- Start with entry points (main files, index files)
- Review module/package organization
- Check dependency management
- Examine test files if available

### Step 2: Quality Dimensions Analysis

Analyze code across these key dimensions:

#### 2.1 Code Smells Detection

**Common Code Smells to Identify:**

**Bloaters:**
- Long Method (>50 lines)
- Large Class (>300 lines or >10 methods)
- Primitive Obsession (overuse of primitives instead of objects)
- Long Parameter List (>3-4 parameters)
- Data Clumps (groups of variables passed together)

**Object-Orientation Abusers:**
- Switch/Case statements (should use polymorphism)
- Temporary Field (fields used only in certain cases)
- Refused Bequest (subclass doesn't use inherited methods)
- Alternative Classes with Different Interfaces

**Change Preventers:**
- Divergent Change (one class changes for multiple reasons)
- Shotgun Surgery (one change requires many small changes)
- Parallel Inheritance Hierarchies

**Dispensables:**
- Comments (excessive or outdated comments)
- Duplicate Code
- Lazy Class (class doing too little)
- Data Class (class with only getters/setters)
- Dead Code (unused code)
- Speculative Generality (unused abstractions)

**Couplers:**
- Feature Envy (method using more features from another class)
- Inappropriate Intimacy (excessive coupling between classes)
- Message Chains (a.getB().getC().getD())
- Middle Man (class delegating all work)

#### 2.2 Complexity Analysis

**Cyclomatic Complexity:**
- Calculate decision points (if, for, while, case, &&, ||)
- **Low Risk**: Complexity 1-10
- **Moderate Risk**: Complexity 11-20
- **High Risk**: Complexity 21-50
- **Very High Risk**: Complexity >50

**Cognitive Complexity:**
- Assess how difficult code is to understand
- Identify nested conditions, loops, recursion
- Flag methods with high cognitive load

**Example Analysis:**
```python
# High Cyclomatic Complexity (>15)
def process_order(order):
    if order.type == 'standard':
        if order.priority == 'high':
            if order.value > 1000:
                # ... nested logic
            elif order.value > 500:
                # ... more conditions
        elif order.priority == 'normal':
            # ... more branches
    elif order.type == 'express':
        # ... even more conditions
    # Recommendation: Extract to smaller methods or use strategy pattern
```

#### 2.3 Maintainability Assessment

**Maintainability Index (MI):**
- Calculate based on: MI = 171 - 5.2 * ln(Halstead Volume) - 0.23 * (Cyclomatic Complexity) - 16.2 * ln(Lines of Code)
- **Good**: MI > 85
- **Moderate**: MI 65-85
- **Difficult**: MI < 65

**Key Factors:**
- Code readability (clear naming, logical structure)
- Modularity (separation of concerns)
- Documentation quality
- Test coverage
- Dependency management

#### 2.4 Naming Conventions

**Check for:**
- Consistent naming style (camelCase, snake_case, PascalCase)
- Descriptive names (avoid single letters except loop counters)
- Appropriate length (not too short, not too long)
- Meaningful abbreviations only
- Boolean names starting with is/has/can/should
- Function names as verbs, class names as nouns

**Examples:**

❌ **Poor Naming:**
```javascript
function proc(d) {  // Unclear function name and parameter
    let x = d * 2;
    return x;
}

class mgr {  // Non-descriptive class name
    // ...
}
```

✅ **Good Naming:**
```javascript
function calculateDiscountedPrice(originalPrice) {
    const discountMultiplier = 2;
    return originalPrice * discountMultiplier;
}

class OrderManager {
    // ...
}
```

#### 2.5 Code Duplication

**Detection:**
- Identify duplicate code blocks (>6 lines)
- Look for similar logic with minor variations
- Check for copy-paste patterns

**Metrics:**
- Calculate duplication percentage
- Identify duplication hotspots

**Recommendation Template:**
```
Found: 3 instances of duplicate code (45 lines total)
Location 1: [file1.js#L120-L165]
Location 2: [file2.js#L89-L134]
Location 3: [file3.js#L201-L246]

Recommendation: Extract common logic to shared utility function
Potential name: formatAndValidateUserInput()
Expected reduction: 135 lines → 45 lines (67% reduction)
```

#### 2.6 Design Patterns and Architecture

**Evaluate:**
- Appropriate use of design patterns (Strategy, Factory, Observer, etc.)
- SOLID principles adherence:
  - Single Responsibility Principle
  - Open/Closed Principle
  - Liskov Substitution Principle
  - Interface Segregation Principle
  - Dependency Inversion Principle
- Separation of concerns
- Dependency injection usage
- Layer separation (presentation, business, data)

**Anti-patterns to Flag:**
- God Object (class doing everything)
- Spaghetti Code (tangled dependencies)
- Golden Hammer (overusing one pattern)
- Cargo Cult Programming (code without understanding)
- Hard Coding (values that should be configurable)

#### 2.7 Error Handling

**Check for:**
- Consistent error handling strategy
- Appropriate exception types
- Error messages quality (descriptive, actionable)
- Resource cleanup (try-finally, context managers)
- Graceful degradation
- Logging at appropriate levels

**Examples:**

❌ **Poor Error Handling:**
```python
def read_file(filename):
    f = open(filename)  # No error handling, resource leak
    data = f.read()
    return data
```

✅ **Good Error Handling:**
```python
def read_file(filename):
    try:
        with open(filename, 'r') as f:
            return f.read()
    except FileNotFoundError:
        logger.error(f"File not found: {filename}")
        raise
    except PermissionError:
        logger.error(f"Permission denied: {filename}")
        raise
    except Exception as e:
        logger.error(f"Unexpected error reading {filename}: {e}")
        raise
```

#### 2.8 Performance Considerations

**Review:**
- Algorithm efficiency (O(n), O(n²), etc.)
- Database query optimization (N+1 queries, missing indexes)
- Memory usage patterns
- Unnecessary computations
- Caching opportunities
- Resource pooling

**Common Issues:**
```javascript
// ❌ O(n²) - inefficient
users.forEach(user => {
    orders.forEach(order => {
        if (order.userId === user.id) {
            // process
        }
    });
});

// ✅ O(n) - efficient with Map
const ordersByUser = new Map();
orders.forEach(order => {
    if (!ordersByUser.has(order.userId)) {
        ordersByUser.set(order.userId, []);
    }
    ordersByUser.get(order.userId).push(order);
});
users.forEach(user => {
    const userOrders = ordersByUser.get(user.id) || [];
    // process
});
```

### Step 3: Language-Specific Analysis

Apply language-specific best practices:

**JavaScript/TypeScript:**
- Use strict mode
- Avoid var, prefer const/let
- Use async/await over callbacks
- Proper promise error handling
- Type safety (TypeScript)
- Avoid implicit any

**Python:**
- PEP 8 compliance
- Type hints usage
- List comprehensions appropriately
- Context managers for resources
- Avoid mutable default arguments
- Virtual environment usage

**Java:**
- Proper use of collections
- Stream API usage
- Exception hierarchy
- Thread safety
- Resource management (try-with-resources)
- Immutability where appropriate

**Go:**
- Error handling patterns
- Goroutine management
- Defer usage
- Interface design
- Package organization
- Effective Go guidelines

**C#:**
- LINQ usage
- Async/await patterns
- IDisposable implementation
- Nullable reference types
- Dependency injection
- .NET conventions

### Step 4: Documentation Quality

**Assess:**
- Code comments (when needed, not excessive)
- Function/method documentation
- Class/module documentation
- API documentation
- README quality
- Inline documentation for complex logic

**Guidelines:**
- Comments explain WHY, not WHAT
- Public APIs fully documented
- Complex algorithms explained
- TODOs tracked and dated
- No commented-out code

### Step 5: Test Quality Assessment

**Review:**
- Test coverage percentage
- Test organization (unit, integration, e2e)
- Test naming conventions
- Assertion quality
- Test data management
- Mock usage appropriateness
- Test maintainability

### Step 6: Generate Review Report
