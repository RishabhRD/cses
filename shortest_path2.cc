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

using graph_t = std::vector<std::vector<std::pair<ll, ll>>>;

auto build_graph(std::vector<std::tuple<ll, ll, ll>> const &edges, ll n) {
  graph_t graph(n + 1);
  for (auto [a, b, w] : edges) {
    graph[a].push_back({b, w});
    graph[b].push_back({a, w});
  }
  return graph;
}

ll sum(ll a, ll b) {
  if (a == INT64_MAX || b == INT64_MAX)
    return INT64_MAX;
  return a + b;
}

auto floydWarshall(graph_t const &graph) {
  auto const n = std::size(graph);
  std::vector dist(n, std::vector(n, ll(INT64_MAX)));
  for (ll i = 1; i < n; ++i)
    dist[i][i] = 0;
  for (ll u = 1; u < n; ++u) {
    for (auto const [v, w] : graph[u]) {
      dist[u][v] = std::min(dist[u][v], w);
    }
  }
  for (ll k = 1; k < n; ++k) {
    for (ll i = 1; i < n; ++i) {
      for (ll j = 1; j < n; ++j) {
        dist[i][j] = std::min(dist[i][j], sum(dist[i][k], dist[k][j]));
      }
    }
  }
  return dist;
}

auto solve(std::vector<std::tuple<ll, ll, ll>> const &edges, ll n,
           std::vector<std::pair<ll, ll>> const &queries) {
  auto const graph = build_graph(edges, n);
  auto const shortest_paths = floydWarshall(graph);
  for (auto const [x, y] : queries) {
    if (shortest_paths[x][y] == INT64_MAX)
      std::cout << -1 << std::endl;
    else
      std::cout << shortest_paths[x][y] << std::endl;
  }
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  auto const q = read<ll>();
  std::vector<std::tuple<ll, ll, ll>> edges;
  for (ll i = 0; i < m; ++i) {
    auto const a = read<ll>();
    auto const b = read<ll>();
    auto const w = read<ll>();
    edges.push_back({a, b, w});
  }
  std::vector<std::pair<ll, ll>> queries;
  for (ll i = 0; i < q; ++i) {
    auto const a = read<ll>();
    auto const b = read<ll>();
    queries.push_back({a, b});
  }
  solve(edges, n, queries);
}
