open Tjr_monad.Types

type ('k,'v,'t) map_ops = {
  find: 'k -> ('v option,'t) m;
  insert: 'k -> 'v -> (unit,'t) m;
  delete: 'k -> (unit,'t)m;
}
