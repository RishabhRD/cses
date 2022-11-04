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

template <typename T> T read() {
  T t;
  std::cin >> t;
  return t;
}

template <typename T> std::vector<T> read_vec(int n) {
  std::vector<T> vec(n);
  for (auto &ele : vec)
    std::cin >> ele;
  return vec;
}

template <typename T> std::vector<T> read_matrix(int m, int n) {
  std::vector<std::vector<T>> vec(m, std::vector<T>(n));
  for (int i = 0; i < m; ++i) {
    for (int j = 0; j < n; ++j) {
      std::cin >> vec[i][j];
    }
  }
  return vec;
}

using ll = long long;

auto solve(std::vector<ll> &as, std::vector<ll> &bs, ll k) {
  std::sort(std::begin(as), std::end(as));
  std::sort(std::begin(bs), std::end(bs));
  auto compare = [k](auto a, auto b) {
    if (a - k <= b && b <= a + k)
      return 0;
    else if (b < a - k)
      return -1;
    else
      return 1;
  };
  ll i = 0;
  ll j = 0;
  ll ans = 0;
  while (i < std::size(as) and j < std::size(bs)) {
    auto cmp = compare(as[i], bs[j]);
    if (cmp == 0) {
      ++ans;
      ++i;
      ++j;
    } else if (cmp == -1) {
      ++j;
    } else {
      ++i;
    }
  }
  return ans;
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  auto const k = read<ll>();
  auto as = read_vec<ll>(n);
  auto bs = read_vec<ll>(m);
  std::cout << solve(as, bs, k) << std::endl;
}
