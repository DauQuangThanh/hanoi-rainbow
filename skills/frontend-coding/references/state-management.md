# State Management


### Zustand (React)

```typescript
import create from 'zustand';
import { devtools, persist } from 'zustand/middleware';

interface Todo {
  id: string;
  text: string;
  completed: boolean;
}

interface TodoStore {
  todos: Todo[];
  filter: 'all' | 'active' | 'completed';
  
  addTodo: (text: string) => void;
  toggleTodo: (id: string) => void;
  deleteTodo: (id: string) => void;
  setFilter: (filter: TodoStore['filter']) => void;
  
  // Computed
  filteredTodos: () => Todo[];
  completedCount: () => number;
}

export const useTodoStore = create<TodoStore>()(
  devtools(
    persist(
      (set, get) => ({
        todos: [],
        filter: 'all',

        addTodo: (text) => set((state) => ({
          todos: [
            ...state.todos,
            {
              id: Date.now().toString(),
              text,
              completed: false
            }
          ]
        })),

        toggleTodo: (id) => set((state) => ({
          todos: state.todos.map(todo =>
            todo.id === id
              ? { ...todo, completed: !todo.completed }
              : todo
          )
        })),

        deleteTodo: (id) => set((state) => ({
          todos: state.todos.filter(todo => todo.id !== id)
        })),

        setFilter: (filter) => set({ filter }),

        filteredTodos: () => {
          const { todos, filter } = get();
          if (filter === 'active') return todos.filter(t => !t.completed);
          if (filter === 'completed') return todos.filter(t => t.completed);
          return todos;
        },

        completedCount: () => {
          return get().todos.filter(t => t.completed).length;
        }
      }),
      { name: 'todo-storage' }
    )
  )
);

// Usage
function TodoApp() {
  const { todos, filter, addTodo, toggleTodo, deleteTodo, setFilter, filteredTodos } = useTodoStore();

  return (
    <div>
      <AddTodoForm onAdd={addTodo} />
      <FilterButtons current={filter} onChange={setFilter} />
      <TodoList
        todos={filteredTodos()}
        onToggle={toggleTodo}
        onDelete={deleteTodo}
      />
    </div>
  );
}
```

### React Query (Server State)

```typescript
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';

interface User {
  id: string;
  name: string;
  email: string;
}

// API functions
const userApi = {
  getAll: () => fetch('/api/users').then(r => r.json()),
  getById: (id: string) => fetch(`/api/users/${id}`).then(r => r.json()),
  create: (user: Omit<User, 'id'>) =>
    fetch('/api/users', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(user)
    }).then(r => r.json()),
  update: (id: string, updates: Partial<User>) =>
    fetch(`/api/users/${id}`, {
      method: 'PATCH',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(updates)
    }).then(r => r.json())
};

// Hooks
function useUsers() {
  return useQuery({
    queryKey: ['users'],
    queryFn: userApi.getAll,
    staleTime: 5 * 60 * 1000, // 5 minutes
    cacheTime: 10 * 60 * 1000, // 10 minutes
  });
}

function useUser(id: string) {
  return useQuery({
    queryKey: ['users', id],
    queryFn: () => userApi.getById(id),
    enabled: !!id, // Only run if id is provided
  });
}

function useCreateUser() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: userApi.create,
    onSuccess: () => {
      // Invalidate and refetch users
      queryClient.invalidateQueries({ queryKey: ['users'] });
    },
  });
}

function useUpdateUser() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: ({ id, updates }: { id: string; updates: Partial<User> }) =>
      userApi.update(id, updates),
    onSuccess: (data, variables) => {
      // Update cache immediately
      queryClient.setQueryData(['users', variables.id], data);
      // Invalidate list
      queryClient.invalidateQueries({ queryKey: ['users'] });
    },
  });
}

// Component
function UserList() {
  const { data: users, isLoading, error } = useUsers();
  const createUser = useCreateUser();
  const updateUser = useUpdateUser();

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  return (
    <div>
      <button
        onClick={() => createUser.mutate({ name: 'New User', email: 'new@example.com' })}
        disabled={createUser.isPending}
      >
        {createUser.isPending ? 'Creating...' : 'Add User'}
      </button>

      <ul>
        {users?.map(user => (
          <li key={user.id}>
            {user.name}
            <button
              onClick={() => updateUser.mutate({
                id: user.id,
                updates: { name: 'Updated Name' }
              })}
            >
              Update
            </button>
          </li>
        ))}
      </ul>
    </div>
  );
}
```
