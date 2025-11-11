use std::collections::HashMap;

#[derive(Debug)]
pub struct SymbolTable<K, V> {
    scopes: Vec<ScopeStack<K, V>>,
    current_scope: usize,
}

type ScopeStack<K, V> = Vec<Scope<K, V>>;

#[derive(Debug)]
struct Scope<K, V> {
    map: HashMap<K, V>,
}

impl<K, V> SymbolTable<K, V>
where
    K: std::hash::Hash + Eq,
{
    pub fn new() -> Self {
        let mut scopes = Vec::new();
        // global scope
        let mut stack = Vec::new();
        stack.push(Scope { map: HashMap::new() });
        scopes.push(stack);

        Self {
            scopes,
            current_scope: 0,
        }
    }

    pub fn push_scope(&mut self) {
        let next_scope = self.current_scope + 1;

        if next_scope >= self.scopes.len() {
            let mut stack = Vec::new();
            stack.push(Scope { map: HashMap::new() });
            self.scopes.push(stack);
        } else {
            self.scopes[next_scope].push(Scope { map: HashMap::new() });
        }

        self.current_scope = next_scope;
    }

    pub fn pop_scope(&mut self) {
        if self.current_scope == 0 {
            // trying to access out of global scope
            panic!("Cannot pop global scope");
        }

        self.current_scope -= 1;
    }

    pub fn define(&mut self, key: K, value: V) {
        let cur_scope = &mut self.scopes[self.current_scope];
        let cur_stack = cur_scope.last_mut().unwrap();
        cur_stack.map.insert(key, value);
    }

    pub fn lookup(&self, key: &K) -> Option<&V> {
        let mut looking_scope = Some(self.current_scope);

        while let Some(scope_idx) = looking_scope {
            if let Some(cur_stack) = self.scopes.get(scope_idx) {
                if !cur_stack.is_empty() {
                    if let Some(scope) = cur_stack.last() {
                        if let Some(value) = scope.map.get(key) {
                            return Some(value);
                        }
                    }
                }
            }

            looking_scope = scope_idx.checked_sub(1);
        }

        None
    }
}

impl<K, V> Default for SymbolTable<K, V>
where
    K: std::hash::Hash + Eq,
{
    fn default() -> Self { Self::new() }
}
