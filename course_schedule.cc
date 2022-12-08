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

enum class status_t { not_visited, visited, visiting };

void topological_sort(graph_t const &graph, ll i, std::vector<bool> &visited,
                      std::vector<ll> &res) {
  if (visited[i])
    return;
  visited[i] = true;
  for (auto n : graph[i])
    topological_sort(graph, n, visited, res);
  res.push_back(i);
}

std::vector<ll> topological_sort(graph_t const &graph) {
  auto const n = std::size(graph);
  std::vector visited(n, false);
  std::vector<ll> res;
  for (ll i = 1; i < n; ++i) {
    topological_sort(graph, i, visited, res);
  }
  std::reverse(std::begin(res), std::end(res));
  return res;
}

bool has_cycle(graph_t const &graph, ll i, std::vector<status_t> &status) {
  if (status[i] == status_t::visited)
    return false;
  if (status[i] == status_t::visiting)
    return true;
  status[i] = status_t::visiting;
  auto const cur_has_cycle =
      std::any_of(std::begin(graph[i]), std::end(graph[i]),
                  [&](auto n) { return has_cycle(graph, n, status); });
  status[i] = status_t::visited;
  return cur_has_cycle;
}

bool has_cycle(graph_t const &graph) {
  auto const n = std::size(graph);
  std::vector status(n, status_t::not_visited);
  for (ll i = 1; i < n; ++i) {
    if (status[i] == status_t::not_visited && has_cycle(graph, i, status))
      return true;
  }
  return false;
}

auto build_graph(std::vector<std::pair<ll, ll>> const &edges, ll n) {
  graph_t graph(n + 1);
  for (auto [a, b] : edges) {
    graph[a].push_back(b);
  }
  return graph;
}

auto solve(std::vector<std::pair<ll, ll>> const &edges, ll n) {
  auto const graph = build_graph(edges, n);
  if (has_cycle(graph)) {
    std::cout << "IMPOSSIBLE" << std::endl;
  } else {
    for (auto n : topological_sort(graph)) {
      std::cout << n << ' ';
    }
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
