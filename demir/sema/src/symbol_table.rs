use std::collections::HashMap;

pub struct SymbolTable<K, V> {
    scopes: Vec<Scope<K, V>>,
}

impl<K: std::fmt::Debug, V: std::fmt::Debug> std::fmt::Debug for SymbolTable<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SymbolTable").field("scopes", &self.scopes).finish()
    }
}

#[derive(Debug)]
struct Scope<K, V> {
    symbols: HashMap<K, V>,
}

impl<K, V> SymbolTable<K, V>
where
    K: Eq + std::hash::Hash,
{
    pub fn new() -> Self {
        let global_scope = vec![Scope {
            symbols: HashMap::new(),
        }];

        Self { scopes: global_scope }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope {
            symbols: HashMap::new(),
        });
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() <= 1 {
            panic!("Cannot pop global scope");
        }
        self.scopes.pop();
    }

    pub fn define(&mut self, key: K, value: V) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.symbols.insert(key, value);
        }
    }

    pub fn lookup(&self, key: &K) -> Option<&V> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.symbols.get(key) {
                return Some(value);
            }
        }
        None
    }
}

impl<K, V> Default for SymbolTable<K, V>
where
    K: Eq + std::hash::Hash,
{
    fn default() -> Self { Self::new() }
}

