# Common Patterns


### Repository Pattern
```javascript
class UserRepository {
  async findById(id) {
    return await db.users.findOne({ id });
  }
  
  async findByEmail(email) {
    return await db.users.findOne({ email });
  }
  
  async create(userData) {
    return await db.users.insert(userData);
  }
  
  async update(id, data) {
    return await db.users.update({ id }, data);
  }
  
  async delete(id) {
    return await db.users.delete({ id });
  }
}
```

### Service Layer Pattern
```javascript
class UserService {
  constructor(userRepository, emailService) {
    this.userRepository = userRepository;
    this.emailService = emailService;
  }
  
  async registerUser(userData) {
    // Validate
    this.validateUserData(userData);
    
    // Check existing
    const existing = await this.userRepository.findByEmail(userData.email);
    if (existing) {
      throw new Error('Email already exists');
    }
    
    // Hash password
    userData.password = await bcrypt.hash(userData.password, 10);
    
    // Create user
    const user = await this.userRepository.create(userData);
    
    // Send welcome email
    await this.emailService.sendWelcomeEmail(user.email);
    
    return user;
  }
}
```
