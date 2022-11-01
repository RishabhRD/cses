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

ll get_square_strip(ll const x, ll const y) { return std::max(x, y); }

ll down_to_up(ll const x, ll const y, ll n) {
  auto const prev_max = (n - 1) * (n - 1);
  if (x == n) {
    return prev_max + y;
  } else {
    return prev_max + n + (n - x);
  }
}

ll up_to_down(ll x, ll y, ll n) {
  auto const prev_max = (n - 1) * (n - 1);
  if (y == n) {
    return prev_max + x;
  } else {
    return prev_max + n + (n - y);
  }
}

ll get_value(ll const x, ll const y) {
  auto const n = get_square_strip(x, y);
  if (n % 2 == 0) {
    return up_to_down(x, y, n);
  } else {
    return down_to_up(x, y, n);
  }
}

int main() {
  auto t = read<int>();
  while (t--) {
    auto const x = read<ll>();
    auto const y = read<ll>();
    std::cout << get_value(x, y) << std::endl;
  }
}
