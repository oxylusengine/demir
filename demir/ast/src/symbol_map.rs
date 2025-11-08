use std::collections::HashMap;

pub struct SymbolMap<K, V, M> {
    scopes: Vec<Vec<Scope<K, V, M>>>,
    current_scope: usize,
}

struct Scope<K, V, M> {
    map: HashMap<K, V>,
    begin_marker: M,
    end_marker: M,
}

impl<K, V, M> SymbolMap<K, V, M>
where
    K: Eq + std::hash::Hash,
    M: Default,
{
    pub fn new() -> Self {
        // Global scope
        let scopes = vec![vec![Scope {
            map: HashMap::new(),
            begin_marker: M::default(),
            end_marker: M::default(),
        }]];

        Self {
            scopes,
            current_scope: 0,
        }
    }

    pub fn push_scope(&mut self, begin_marker: M, end_marker: M) {
        let next_scope = self.current_scope + 1;

        if next_scope >= self.scopes.len() {
            let stack = vec![Scope {
                map: HashMap::new(),
                begin_marker,
                end_marker,
            }];
            self.scopes.push(stack);
        } else {
            self.scopes[next_scope].push(Scope {
                map: HashMap::new(),
                begin_marker,
                end_marker,
            });
        }

        self.current_scope = next_scope;
    }

    pub fn pop_scope(&mut self) {
        if self.current_scope == 0 {
            panic!("Cannot pop global scope");
        }

        self.current_scope -= 1;
    }

    pub fn add_symbol(&mut self, key: K, value: V, target_scope: Option<usize>) {
        let scope_idx = target_scope.unwrap_or(self.current_scope);
        let cur_scope = &mut self.scopes[scope_idx];
        let cur_stack = cur_scope.last_mut().unwrap();
        cur_stack.map.insert(key, value);
    }

    pub fn lookup(&self, key: &K, target_scope: Option<usize>) -> Option<&V> {
        let mut looking_scope = target_scope.unwrap_or(self.current_scope);

        loop {
            if let Some(cur_stack) = self.scopes.get(looking_scope)
                && let Some(scope) = cur_stack.last()
                && let Some(value) = scope.map.get(key)
            {
                return Some(value);
            }

            if looking_scope == 0 {
                break;
            }
            looking_scope -= 1;
        }

        None
    }

    pub fn current_scope_markers(&self) -> (&M, &M) {
        let cur_scope = &self.scopes[self.current_scope];
        let cur_stack = cur_scope.last().unwrap();

        (&cur_stack.begin_marker, &cur_stack.end_marker)
    }
}

impl<K, V, M> Default for SymbolMap<K, V, M>
where
    K: Eq + std::hash::Hash,
    M: Default,
{
    fn default() -> Self { Self::new() }
}
