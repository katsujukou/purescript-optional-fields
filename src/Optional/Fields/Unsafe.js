export function unsafeGet_(None, Just, p, r) {
  if (r[p] == void 0) return None;
  return Just(r[p]);
}

// export function unsafeInsert_(r, p, v) {
//   return { ...r, [p]: v };
// }
