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
  }
  return graph;
}

auto build_op_graph(std::vector<std::tuple<ll, ll, ll>> const &edges, ll n) {
  graph_t graph(n + 1);
  for (auto [a, b, w] : edges) {
    graph[b].push_back({a, w});
  }
  return graph;
}

ll sum(ll a, ll b) {
  if (a == INT64_MAX || b == INT64_MAX)
    return INT64_MAX;
  return a + b;
}

auto dijkstra(graph_t const &graph, ll src) {
  auto const n = std::size(graph);
  using pii = std::pair<ll, ll>;
  std::priority_queue<pii, std::vector<pii>, std::greater<pii>> pq;
  std::vector<ll> dist(n, INT64_MAX);
  dist[src] = 0;
  pq.push({0, src});
  while (not std::empty(pq)) {
    auto const [du, u] = pq.top();
    pq.pop();
    if (du > dist[u])
      continue;
    for (auto const [v, w] : graph[u]) {
      if (dist[v] > dist[u] + w) {
        dist[v] = dist[u] + w;
        pq.push({dist[v], v});
      }
    }
  }
  return dist;
}

auto solve(std::vector<std::tuple<ll, ll, ll>> edges, ll n) {
  auto const graph = build_graph(edges, n);
  auto const op_graph = build_op_graph(edges, n);
  auto const dist = dijkstra(graph, 1);
  auto const op_dist = dijkstra(op_graph, n);
  ll ans = INT64_MAX;
  for (ll k = 1; k <= n; ++k) {
    for (auto [n, w] : graph[k]) {
      ans = std::min(ans, sum(sum(dist[k], (w / 2)), op_dist[n]));
    }
  }
  std::cout << ans << std::endl;
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  std::vector<std::tuple<ll, ll, ll>> edges;
  for (ll i = 0; i < m; ++i) {
    auto const a = read<ll>();
    auto const b = read<ll>();
    auto const w = read<ll>();
    edges.push_back({a, b, w});
  }
  solve(edges, n);
}
