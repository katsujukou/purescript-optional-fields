import { Just, Nothing } from "../Data.Maybe/index.js";

export function unsafeGet_(p, r) {
  if (r[p] == void 0) return Nothing.value;
  if (r[p] instanceof Nothing || r[p] instanceof Just) {
    return r[p];
  }
  return Just.create(r[p]);
}
