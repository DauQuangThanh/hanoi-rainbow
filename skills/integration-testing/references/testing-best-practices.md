# Testing Best Practices


### Test Organization
1. **Arrange-Act-Assert (AAA) Pattern**
   ```javascript
   it('should do something', () => {
     // Arrange - Set up test data
     const input = { value: 10 };
     
     // Act - Execute the test
     const result = doSomething(input);
     
     // Assert - Verify the result
     expect(result).toBe(20);
   });
   ```

2. **Test Independence**
   - Each test should be isolated
   - Use beforeEach/afterEach for setup/cleanup
   - Don't rely on test execution order

3. **Descriptive Test Names**
   ```javascript
   // ❌ Bad
   it('works', () => { });
   
   // ✅ Good
   it('should return 404 when user does not exist', () => { });
   ```

### Test Data
1. **Use factories for dynamic data**
2. **Use fixtures for stable reference data**
3. **Clean up test data after each test**
4. **Avoid hardcoded IDs**
5. **Use meaningful test data**

### Performance
1. **Run unit tests before integration tests**
2. **Use test database snapshots**
3. **Parallel test execution where possible**
4. **Mock slow external dependencies**
5. **Set appropriate timeouts**

### Maintenance
1. **Keep tests simple and focused**
2. **Avoid test duplication**
3. **Refactor tests with code**
4. **Monitor test flakiness**
5. **Remove obsolete tests**
