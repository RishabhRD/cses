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

template <typename T, typename Op> T gen_power(T n, ll x, T unit, Op &&op) {
  if (x == 0)
    return unit;
  auto ans = gen_power(n, x / 2, unit, op);
  ans = op(ans, ans);
  if (x % 2)
    ans = op(n, ans);
  return ans;
}

auto make_mod_add(ll mod) {
  return [mod](ll a, ll b) { return (a + b) % mod; };
}

auto make_mod_minus(ll mod) {
  return [mod](ll a, ll b) { return (a - b + mod) % mod; };
}

auto make_mod_multiply(ll mod) {
  return [mod](ll a, ll b) { return (a * b) % mod; };
}

auto mod_add = make_mod_add(mod);
auto mod_minus = make_mod_minus(mod);
auto mod_multiply = make_mod_multiply(mod);

template <typename T, typename BiOp = std::multiplies<>,
          typename FoldOp = std::plus<>>
auto matrix_multiply(std::vector<std::vector<T>> const &a,
                     std::vector<std::vector<T>> const &b,
                     BiOp &&biop = std::multiplies<>{},
                     FoldOp &&foldop = std::plus<>{}) {
  auto const n = std::size(a);
  std::vector<std::vector<ll>> matrix(n, std::vector<ll>(n));
  for (ll i = 0; i < n; ++i) {
    for (ll j = 0; j < n; ++j) {
      for (ll k = 0; k < n; ++k) {
        matrix[i][j] = foldop(matrix[i][j], biop(a[i][k], b[k][j]));
      }
    }
  }
  return matrix;
}

template <typename BiOp = std::multiplies<>, typename FoldOp = std::plus<>>
auto make_matrix_multiply(BiOp &&biop = std::multiplies<>{},
                          FoldOp &&foldop = std::plus<>{}) {
  return [biop, foldop](auto const &a, auto const &b) {
    return matrix_multiply(a, b, biop, foldop);
  };
}

auto solve(ll sum) {
  std::vector init(6, std::vector(6, 0ll));
  for (ll i = 0; i < 5; ++i) {
    init[i][i + 1] = 1;
  }
  for (ll i = 0; i < 6; ++i) {
    init[5][i] = 1;
  }
  std::vector<std::vector<ll>> unit{
      {1, 0, 0, 0, 0, 0}, {0, 1, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0},
      {0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 1},
  };
  auto const matrix =
      gen_power(init, sum, unit, make_matrix_multiply(mod_multiply, mod_add));
  return matrix[5][5];
}

int main() {
  auto const n = read<ll>();
  std::cout << solve(n) << std::endl;
}
