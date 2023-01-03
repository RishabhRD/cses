#include <algorithm>
#include <array>
#include <bitset>
#include <chrono>
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

struct custom_hash {
  static uint64_t splitmix64(uint64_t x) {
    x += 0x9e3779b97f4a7c15;
    x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9;
    x = (x ^ (x >> 27)) * 0x94d049bb133111eb;
    return x ^ (x >> 31);
  }

  size_t operator()(uint64_t x) const {
    static const uint64_t FIXED_RANDOM =
        std::chrono::steady_clock::now().time_since_epoch().count();
    return splitmix64(x + FIXED_RANDOM);
  }
};

using ll = long long;
constexpr ll mod = 1e9 + 7;

using safe_set = std::unordered_set<ll, custom_hash>;

template <typename T> using safe_map = std::unordered_map<ll, T>;

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

template <typename T> auto read_matrix(int m, int n) {
  std::vector<std::vector<T>> vec(m, std::vector<T>(n));
  for (int i = 0; i < m; ++i) {
    for (int j = 0; j < n; ++j) {
      std::cin >> vec[i][j];
    }
  }
  return vec;
}

struct matrix {
  ll first;
  ll second;
  ll third;
  ll fourth;
};

ll power(ll n, ll x) {
  if (x == 0)
    return 1;
  auto ans = power(n, x / 2);
  ans = (ans * ans) % mod;
  if (x % 2)
    ans = (ans * n) % mod;
  return ans;
}

template <typename T, typename Op> T gen_power(T n, ll x, T unit, Op &&op) {
  if (x == 0)
    return unit;
  auto ans = gen_power(n, x / 2, unit, op);
  ans = op(ans, ans);
  if (x % 2)
    ans = op(n, ans);
  return ans;
}

matrix multiply(matrix a, matrix b) {
  return {
      ((a.first * b.first) % mod + (a.second * b.third) % mod) % mod,
      ((a.first * b.second) % mod + (a.second * b.fourth) % mod) % mod,
      ((a.third * b.first) % mod + (a.fourth * b.third) % mod) % mod,
      ((a.third * b.second) % mod + (a.fourth * b.fourth) % mod) % mod,
  };
}

auto solve(ll n) {
  if (n == 0)
    std::cout << 0 << std::endl;
  else {
    matrix init{0, 1, 1, 1};
    auto const [_, __, ___, a4] =
        gen_power(init, n - 1, matrix{1, 0, 0, 1}, multiply);
    std::cout << a4 << std::endl;
  }
}

int main() {
  auto const n = read<ll>();
  solve(n);
}
