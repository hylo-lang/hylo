namespace ValStdLib {
  // TODO: remove these, after properly translating them from Val
  inline bool operator <(Int l, Int r) { return l.value < r.value; }
  inline Int operator +(Int l, Int r) { return l.value + r.value; }
  inline Int operator -(Int l, Int r) { return l.value - r.value; }
  inline Int operator *(Int l, Int r) { return l.value * r.value; }

  inline void abort() { ::abort(); }
}
