# Common Refactoring Scenarios


### Legacy Code Refactoring

When working with legacy code without tests:

1. **Add Characterization Tests**
   ```typescript
   // Document current behavior before changing
   describe('Legacy calculatePrice', () => {
     it('returns correct price for scenario A', () => {
       expect(calculatePrice(input1)).toBe(expectedOutput1);
     });
     
     it('returns correct price for scenario B', () => {
       expect(calculatePrice(input2)).toBe(expectedOutput2);
     });
   });
   ```

2. **Identify Seams**
   - Find places where you can inject dependencies
   - Extract pure functions that can be tested in isolation

3. **Refactor Incrementally**
   - Make one small change
   - Run characterization tests
   - Commit if tests pass

### Performance Refactoring

```typescript
// Before: N+1 query problem
async function getPostsWithAuthors() {
  const posts = await Post.findAll();
  
  for (const post of posts) {
    post.author = await User.findById(post.authorId);
  }
  
  return posts;
}

// After: Optimized with eager loading
async function getPostsWithAuthors() {
  return await Post.findAll({
    include: [{ model: User, as: 'author' }]
  });
}

// Or with manual optimization
async function getPostsWithAuthors() {
  const posts = await Post.findAll();
  const authorIds = [...new Set(posts.map(p => p.authorId))];
  const authors = await User.findAll({
    where: { id: { [Op.in]: authorIds } }
  });
  
  const authorMap = new Map(authors.map(a => [a.id, a]));
  
  return posts.map(post => ({
    ...post,
    author: authorMap.get(post.authorId)
  }));
}
```
