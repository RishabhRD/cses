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
constexpr ll inf = 4e18;
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

template <typename T, typename Op> T gen_power_w0(T n, ll x, Op &&op) {
  if (x == 1)
    return n;
  auto ans = gen_power_w0(n, x / 2, op);
  ans = op(ans, ans);
  if (x % 2)
    ans = op(n, ans);
  return ans;
}

template <typename T = ll, typename BiOp = std::multiplies<>,
          typename FoldOp = std::plus<>>
auto matrix_multiply(std::vector<std::vector<T>> const &a,
                     std::vector<std::vector<T>> const &b, T const &init = 0,
                     BiOp &&biop = std::multiplies<>{},
                     FoldOp &&foldop = std::plus<>{}) {
  auto const n = std::size(a);
  std::vector<std::vector<T>> matrix(n, std::vector<T>(n, init));
  for (ll i = 0; i < n; ++i) {
    for (ll j = 0; j < n; ++j) {
      for (ll k = 0; k < n; ++k) {
        matrix[i][j] = foldop(matrix[i][j], biop(a[i][k], b[k][j]));
      }
    }
  }
  return matrix;
}

template <typename T = ll, typename BiOp = std::multiplies<>,
          typename FoldOp = std::plus<>>
auto make_matrix_multiply(T init = 0, BiOp &&biop = std::multiplies<>{},
                          FoldOp &&foldop = std::plus<>{}) {
  return [biop, foldop, init](std::vector<std::vector<T>> const &a,
                              std::vector<std::vector<T>> const &b) {
    return matrix_multiply(a, b, init, biop, foldop);
  };
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  auto const k = read<ll>();
  std::vector graph(n, std::vector<ll>(n, inf));
  for (ll i = 0; i < m; ++i) {
    auto const a = read<ll>() - 1;
    auto const b = read<ll>() - 1;
    auto const w = read<ll>();
    graph[a][b] = std::min<ll>(graph[a][b], w);
  }
  auto const mul = make_matrix_multiply(
      inf, std::plus<>{}, [](ll a, ll b) { return std::min(a, b); });
  ll ans = gen_power_w0(graph, k, mul)[0][n - 1];
  if (ans < inf)
    std::cout << ans << std::endl;
  else
    std::cout << -1 << std::endl;
}
