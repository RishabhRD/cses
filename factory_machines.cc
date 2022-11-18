#include <algorithm>
#include <array>
#include <bitset>
#include <cmath>
#include <deque>
#include <iostream>
#include <iterator>
#include <limits>
#include <map>
#include <numeric>
#include <optional>
#include <queue>
#include <set>
#include <stack>
#include <unordered_map>
#include <unordered_set>
#include <vector>

std::ostream &operator<<(std::ostream &dest, __int128_t value) {
  std::ostream::sentry s(dest);
  if (s) {
    __uint128_t tmp = value < 0 ? -value : value;
    char buffer[128];
    char *d = std::end(buffer);
    do {
      --d;
      *d = "0123456789"[tmp % 10];
      tmp /= 10;
    } while (tmp != 0);
    if (value < 0) {
      --d;
      *d = '-';
    }
    int len = std::end(buffer) - d;
    if (dest.rdbuf()->sputn(d, len) != len) {
      dest.setstate(std::ios_base::badbit);
    }
  }
  return dest;
}

__int128 readll() {
  __int128 x = 0, f = 1;
  char ch = getchar();
  while (ch < '0' || ch > '9') {
    if (ch == '-')
      f = -1;
    ch = getchar();
  }
  while (ch >= '0' && ch <= '9') {
    x = x * 10 + ch - '0';
    ch = getchar();
  }
  return x * f;
}

template <typename T> std::vector<T> read_vec(int n) {
  std::vector<T> vec(n);
  for (auto &ele : vec)
    ele = readll();
  return vec;
}

using ll = __int128;

ll numUnprocessed(std::vector<ll> const &timeNeeded, ll numProducts,
                  ll maxTime) {
  return std::accumulate(
      std::begin(timeNeeded), std::end(timeNeeded), numProducts,
      [maxTime](ll curProducts, ll t) { return curProducts - (maxTime / t); });
}

template <typename Predicate>
ll binary_search(ll low, ll high, Predicate &&is_possible) {
  if (low == high)
    return low;
  auto const mid = low + ((high - low) / 2);
  if (is_possible(mid)) {
    return binary_search(mid + 1, high, is_possible);
  } else {
    return binary_search(low, mid, is_possible);
  }
}

ll solve(std::vector<ll> &timeNeeded, ll numProducts) {
  std::sort(std::begin(timeNeeded), std::end(timeNeeded));
  ll const high = numProducts * timeNeeded.back();
  return binary_search(0, high, [&](ll t) {
    return numUnprocessed(timeNeeded, numProducts, t) > 0;
  });
}

int main() {
  auto const n = readll();
  auto const numProducts = readll();
  auto timeNeeded = read_vec<ll>(n);
  std::cout << solve(timeNeeded, numProducts) << std::endl;
}
