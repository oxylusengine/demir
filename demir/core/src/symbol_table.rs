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
        Self {
            scopes: vec![vec![Scope { map: HashMap::new() }]],
            current_scope: 0,
        }
    }

    pub fn push_scope(&mut self) {
        let next_scope = self.current_scope + 1;
        let new_scope = Scope { map: HashMap::new() };

        if next_scope >= self.scopes.len() {
            self.scopes.push(vec![new_scope]);
        } else {
            self.scopes[next_scope].push(new_scope);
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
        let mut scope_idx = self.current_scope;

        loop {
            match self
                .scopes
                .get(scope_idx)
                .and_then(|cur_stack| cur_stack.last())
                .and_then(|scope| scope.map.get(key))
            {
                Some(value) => return Some(value),
                None => {
                    if scope_idx == 0 {
                        break;
                    }
                    scope_idx = scope_idx.saturating_sub(1);
                },
            }
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
