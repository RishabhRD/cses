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

std::pair<ll, ll> dfs(ll i, ll prev, std::vector<std::vector<ll>> const &tree) {
  ll max_distance = 0;
  ll node = i;
  for (auto n : tree[i]) {
    if (n != prev) {
      auto const [nn, d] = dfs(n, i, tree);
      if (1 + d > max_distance) {
        node = nn;
        max_distance = 1 + d;
      }
    }
  }
  return {node, max_distance};
}

auto solve(ll n, std::vector<std::pair<ll, ll>> const &edges) {
  std::vector<std::vector<ll>> tree(n);
  for (auto [a, b] : edges) {
    tree[a].push_back(b);
    tree[b].push_back(a);
  }
  auto const [node, d] = dfs(0, -1, tree);
  auto const [_, d1] = dfs(node, -1, tree);
  std::cout << d1 << std::endl;
}

int main() {
  auto const n = read<ll>();
  std::vector<std::pair<ll, ll>> edges;
  for (ll i = 0; i < n - 1; ++i) {
    auto const a = read<ll>() - 1;
    auto const b = read<ll>() - 1;
    edges.push_back({a, b});
  }
  solve(n, edges);
}
