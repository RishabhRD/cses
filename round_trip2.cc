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

using graph_t = std::vector<std::vector<ll>>;

auto build_graph(std::vector<std::pair<ll, ll>> const &edges, ll n) {
  graph_t graph(n + 1);
  for (auto [a, b] : edges) {
    graph[a].push_back(b);
  }
  return graph;
}

std::optional<ll> detect_cycle(ll i, graph_t const &graph,
                               std::vector<bool> &visited,
                               std::vector<bool> &visiting) {
  if (visiting[i])
    return i;
  if (visited[i])
    return std::nullopt;
  visited[i] = true;
  visiting[i] = true;
  auto const res = std::accumulate(
      std::begin(graph[i]), std::end(graph[i]), std::optional<ll>(),
      [&](auto cur, auto n) -> std::optional<ll> {
        auto res = detect_cycle(n, graph, visited, visiting);
        if (res.has_value())
          return res.value();
        return cur;
      });
  visiting[i] = false;
  return res;
}

void dfs(ll i, ll target, graph_t const &graph, std::vector<bool> &visited,
         std::vector<ll> &cur, std::vector<ll> &res) {
  if (visited[i])
    return;
  visited[i] = true;
  cur.push_back(i);
  if (i == target) {
    res = cur;
    return;
  }
  for (auto n : graph[i]) {
    dfs(n, target, graph, visited, cur, res);
  }
  cur.pop_back();
}

auto solve(std::vector<std::pair<ll, ll>> const &edges, ll n) {
  auto const graph = build_graph(edges, n);
  auto const op_cycle_node = [&] {
    std::vector visited(n + 1, false);
    std::vector visiting(n + 1, false);
    std::optional<ll> ans;
    for (ll i = 1; i <= n; ++i) {
      if (not visited[i]) {
        auto const res = detect_cycle(i, graph, visited, visiting);
        if (res.has_value()) {
          ans = res;
        }
      }
    }
    return ans;
  }();
  if (!op_cycle_node.has_value()) {
    std::cout << "IMPOSSIBLE" << std::endl;
  } else {
    auto const cycle_node = op_cycle_node.value();
    std::vector<ll> res;
    std::vector<bool> visited(n + 1);
    for (auto n : graph[cycle_node]) {
      std::vector<ll> cur;
      dfs(n, cycle_node, graph, visited, cur, res);
    }
    std::cout << res.size() + 1 << std::endl;
    std::cout << cycle_node << ' ';
    for (auto ele : res)
      std::cout << ele << ' ';
    std::cout << std::endl;
  }
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  std::vector<std::pair<ll, ll>> edges;
  for (ll i = 0; i < m; ++i) {
    auto const a = read<ll>();
    auto const b = read<ll>();
    edges.push_back({a, b});
  }
  solve(edges, n);
}
