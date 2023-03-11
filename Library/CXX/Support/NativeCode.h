namespace Val {
  // TODO: remove these, after properly translating them from Val
  inline bool operator <(Int l, Int r) { return l.value < r.value; }
  inline bool operator ==(Int l, Int r) { return l.value == r.value; }
  inline bool operator !=(Int l, Int r) { return l.value != r.value; }
  inline Int operator +(Int l, Int r) { return l.value + r.value; }
  inline Int operator -(Int l, Int r) { return l.value - r.value; }
  inline Int operator *(Int l, Int r) { return l.value * r.value; }

  inline bool operator ==(Bool l, Bool r) { return l.value == r.value; }
  inline bool operator !=(Bool l, Bool r) { return l.value != r.value; }

  inline void abort() { ::abort(); }
}
