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

auto dijkstra(graph_t const &graph) {
  auto const n = std::size(graph);
  std::vector<ll> dist(n, INT64_MAX);
  std::vector<ll> num_min(n, 0);
  std::vector<ll> minf(n, n);
  std::vector<ll> maxf(n, 0);
  dist[1] = 0;
  num_min[1] = 1;
  minf[1] = 0;
  maxf[1] = 0;
  using pii = std::pair<ll, ll>;
  std::priority_queue<pii, std::vector<pii>, std::greater<>> pq;
  pq.push({0, 1});
  while (not std::empty(pq)) {
    auto const [du, u] = pq.top();
    pq.pop();
    if (du > dist[u])
      continue;
    for (auto const [v, w] : graph[u]) {
      if (dist[u] + w < dist[v]) {
        dist[v] = dist[u] + w;
        num_min[v] = num_min[u];
        minf[v] = 1 + minf[u];
        maxf[v] = 1 + maxf[u];
        pq.push({dist[v], v});
      } else if (dist[u] + w == dist[v]) {
        num_min[v] = (num_min[v] + num_min[u]) % mod;
        minf[v] = std::min(minf[v], 1 + minf[u]);
        maxf[v] = std::max(maxf[v], 1 + maxf[u]);
      }
    }
  }
  return std::tuple{dist[n - 1], num_min[n - 1], minf[n - 1], maxf[n - 1]};
}

auto solve(std::vector<std::tuple<ll, ll, ll>> const &edges, ll n) {
  auto const graph = build_graph(edges, n);
  auto const [a, b, c, d] = dijkstra(graph);
  std::cout << a << ' ' << b << ' ' << c << ' ' << d << std::endl;
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
