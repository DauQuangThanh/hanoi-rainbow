# Vue.js Development


### Composition API

**Modern Vue 3 Component**:
```vue
<script setup lang="ts">
import { ref, computed, watch, onMounted } from 'vue';

interface User {
  id: string;
  name: string;
  email: string;
}

interface Props {
  userId: string;
}

const props = defineProps<Props>();
const emit = defineEmits<{
  (e: 'update', user: User): void;
  (e: 'error', error: Error): void;
}>();

// Reactive state
const user = ref<User | null>(null);
const loading = ref(true);
const error = ref<Error | null>(null);

// Computed property
const displayName = computed(() => {
  if (!user.value) return '';
  return `${user.value.name} (${user.value.email})`;
});

// Methods
async function fetchUser() {
  try {
    loading.value = true;
    error.value = null;
    const response = await fetch(`/api/users/${props.userId}`);
    if (!response.ok) throw new Error('Failed to fetch user');
    user.value = await response.json();
  } catch (err) {
    error.value = err instanceof Error ? err : new Error('Unknown error');
    emit('error', error.value);
  } finally {
    loading.value = false;
  }
}

function updateUser(updates: Partial<User>) {
  if (!user.value) return;
  
  user.value = { ...user.value, ...updates };
  emit('update', user.value);
}

// Watch for prop changes
watch(() => props.userId, () => {
  fetchUser();
}, { immediate: true });

// Lifecycle
onMounted(() => {
  console.log('Component mounted');
});
</script>

<template>
  <div class="user-profile">
    <div v-if="loading">Loading...</div>
    <div v-else-if="error">Error: {{ error.message }}</div>
    <div v-else-if="!user">User not found</div>
    <div v-else>
      <h2>{{ displayName }}</h2>
      <button @click="updateUser({ name: 'Updated Name' })">
        Update Name
      </button>
    </div>
  </div>
</template>

<style scoped>
.user-profile {
  padding: 1rem;
}
</style>
```

**Composables (Reusable Logic)**:
```typescript
// composables/useAsync.ts
import { ref, Ref } from 'vue';

export function useAsync<T>(
  asyncFunction: () => Promise<T>
): {
  data: Ref<T | null>;
  loading: Ref<boolean>;
  error: Ref<Error | null>;
  execute: () => Promise<void>;
} {
  const data = ref<T | null>(null);
  const loading = ref(false);
  const error = ref<Error | null>(null);

  const execute = async () => {
    try {
      loading.value = true;
      error.value = null;
      data.value = await asyncFunction();
    } catch (err) {
      error.value = err instanceof Error ? err : new Error('Unknown error');
    } finally {
      loading.value = false;
    }
  };

  return { data, loading, error, execute };
}

// composables/useLocalStorage.ts
import { ref, watch, Ref } from 'vue';

export function useLocalStorage<T>(key: string, initialValue: T): Ref<T> {
  const storedValue = ref<T>(initialValue);

  // Initialize from localStorage
  try {
    const item = window.localStorage.getItem(key);
    if (item) {
      storedValue.value = JSON.parse(item);
    }
  } catch (error) {
    console.error(error);
  }

  // Watch for changes and sync to localStorage
  watch(storedValue, (newValue) => {
    try {
      window.localStorage.setItem(key, JSON.stringify(newValue));
    } catch (error) {
      console.error(error);
    }
  }, { deep: true });

  return storedValue;
}

// Usage in component
<script setup lang="ts">
import { useAsync } from '@/composables/useAsync';
import { useLocalStorage } from '@/composables/useLocalStorage';

const preferences = useLocalStorage('user-preferences', {
  theme: 'light',
  language: 'en'
});

const { data: users, loading, execute } = useAsync(() =>
  fetch('/api/users').then(r => r.json())
);

onMounted(() => {
  execute();
});
</script>
```

### Pinia Store

```typescript
// stores/userStore.ts
import { defineStore } from 'pinia';
import { ref, computed } from 'vue';

interface User {
  id: string;
  name: string;
  email: string;
  role: string;
}

export const useUserStore = defineStore('user', () => {
  // State
  const currentUser = ref<User | null>(null);
  const users = ref<User[]>([]);
  const loading = ref(false);

  // Getters
  const isAdmin = computed(() => currentUser.value?.role === 'admin');
  const userCount = computed(() => users.value.length);

  // Actions
  async function fetchCurrentUser() {
    loading.value = true;
    try {
      const response = await fetch('/api/user/me');
      currentUser.value = await response.json();
    } catch (error) {
      console.error('Failed to fetch user', error);
    } finally {
      loading.value = false;
    }
  }

  async function fetchUsers() {
    loading.value = true;
    try {
      const response = await fetch('/api/users');
      users.value = await response.json();
    } catch (error) {
      console.error('Failed to fetch users', error);
    } finally {
      loading.value = false;
    }
  }

  async function updateUser(id: string, updates: Partial<User>) {
    const response = await fetch(`/api/users/${id}`, {
      method: 'PATCH',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(updates)
    });

    if (response.ok) {
      const updated = await response.json();
      const index = users.value.findIndex(u => u.id === id);
      if (index !== -1) {
        users.value[index] = updated;
      }
      if (currentUser.value?.id === id) {
        currentUser.value = updated;
      }
    }
  }

  function logout() {
    currentUser.value = null;
    users.value = [];
  }

  return {
    // State
    currentUser,
    users,
    loading,
    // Getters
    isAdmin,
    userCount,
    // Actions
    fetchCurrentUser,
    fetchUsers,
    updateUser,
    logout
  };
});

// Usage in component
<script setup lang="ts">
import { useUserStore } from '@/stores/userStore';

const userStore = useUserStore();

onMounted(() => {
  userStore.fetchCurrentUser();
});
</script>

<template>
  <div>
    <div v-if="userStore.loading">Loading...</div>
    <div v-else-if="userStore.currentUser">
      <h2>{{ userStore.currentUser.name }}</h2>
      <p v-if="userStore.isAdmin">Admin User</p>
    </div>
  </div>
</template>
```
