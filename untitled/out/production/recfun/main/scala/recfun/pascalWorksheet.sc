def pascal(c: Int, r: Int): Int = {

  if (c < 0 ) throw new IllegalArgumentException

  if (r < 0) throw new IllegalArgumentException

  if (c > r) throw new IllegalArgumentException

  if (c == 0 || r == 0 || r == c) 1 else (pascal(c-1 , r -1) + pascal(c, r -1))
}
