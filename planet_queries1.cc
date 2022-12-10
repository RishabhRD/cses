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

using ll = int;
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

auto kth_neighbor(std::array<std::vector<ll>, 32> const &neighbor, ll n, ll q) {
  auto const bs = std::bitset<32>(q);
  for (ll i = 0; i < 32; ++i) {
    if (bs[i]) {
      n = neighbor[i][n];
    }
  }
  return n;
}

namespace detail {
template <typename T, std::size_t... Is>
constexpr std::array<T, sizeof...(Is)>
create_array(T value, std::index_sequence<Is...>) {
  // cast Is to void to remove the warning: unused value
  return {{(static_cast<void>(Is), value)...}};
}
} // namespace detail

template <std::size_t N, typename T>
constexpr std::array<T, N> create_array(const T &value) {
  return detail::create_array(value, std::make_index_sequence<N>());
}

auto make_binary_exponential_neighbor(std::vector<ll> const &graph) {
  auto const n = std::size(graph);
  auto neighbor = create_array<32>(std::vector<ll>(n));
  neighbor[0] = graph;
  for (ll i = 1; i < 32; ++i) {
    for (ll j = 1; j < n; ++j) {
      neighbor[i][j] = neighbor[i - 1][j];
      neighbor[i][j] = neighbor[i - 1][neighbor[i][j]];
    }
  }
  return neighbor;
}

auto solve(std::vector<ll> const &graph, ll q) {
  auto const bi_exp_neighbor = make_binary_exponential_neighbor(graph);
  for (ll i = 0; i < q; ++i) {
    auto const n = read<ll>();
    auto const k = read<ll>();
    std::cout << kth_neighbor(bi_exp_neighbor, n, k) << std::endl;
  }
}

int main() {
  std::cin.tie(0);
  auto const n = read<ll>();
  auto const q = read<ll>();
  std::vector<ll> graph(n + 1);
  for (ll i = 1; i <= n; ++i)
    std::cin >> graph[i];
  solve(graph, q);
}
