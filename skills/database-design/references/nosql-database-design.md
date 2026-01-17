# NoSQL Database Design


### MongoDB Document Design

```javascript
// Embedded Documents (one-to-few)
{
  _id: ObjectId("..."),
  name: "John Doe",
  email: "john@example.com",
  addresses: [
    { type: "home", street: "123 Main St", city: "NYC" },
    { type: "work", street: "456 Office Blvd", city: "NYC" }
  ]
}

// Reference (one-to-many, many-to-many)
// Users collection
{ _id: ObjectId("user123"), name: "John Doe" }

// Orders collection
{
  _id: ObjectId("order456"),
  user_id: ObjectId("user123"),
  items: [
    { product_id: ObjectId("prod789"), quantity: 2 }
  ]
}

// Design Decision Rules:
// Embed when:
// - Data is accessed together
// - One-to-few relationship
// - Child data doesn't change often
// - Bounded growth (won't exceed 16MB limit)

// Reference when:
// - Data accessed separately
// - One-to-many or many-to-many
// - Frequently updated data
// - Unbounded growth
```
