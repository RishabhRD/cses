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

void dfs1(ll i, std::vector<std::vector<ll>> const &graph,
          std::vector<bool> &visited, std::vector<ll> &order) {
  if (visited[i])
    return;
  visited[i] = true;
  for (auto n : graph[i]) {
    dfs1(n, graph, visited, order);
  }
  order.push_back(i);
}

void dfs2(ll i, std::vector<std::vector<ll>> const &graph, ll cur_group,
          std::vector<ll> &group) {
  if (group[i] != -1)
    return;
  group[i] = cur_group;
  for (auto n : graph[i])
    dfs2(n, graph, cur_group, group);
}

void dfs(ll i, std::vector<std::vector<ll>> const &graph,
         std::vector<bool> &visited) {
  if (visited[i])
    return;
  visited[i] = true;
  for (auto n : graph[i]) {
    dfs(n, graph, visited);
  }
}

auto is_connected(ll n, std::vector<std::pair<ll, ll>> const &edges) {
  std::vector<std::vector<ll>> graph(n);
  std::vector<std::vector<ll>> op_graph(n);
  for (auto [a, b] : edges) {
    graph[a].push_back(b);
    op_graph[b].push_back(a);
  }
  std::vector<ll> order;
  {
    std::vector<bool> visited(n);
    for (ll i = 0; i < n; ++i) {
      dfs1(i, graph, visited, order);
    }
  }
  std::reverse(std::begin(order), std::end(order));
  ll num_group = 0;
  std::vector<ll> group(n, -1);
  {
    for (auto n : order) {
      if (group[n] == -1) {
        dfs2(n, op_graph, num_group, group);
        ++num_group;
      }
    }
  }
  std::vector<std::vector<ll>> super_graph(num_group);
  for (ll i = 0; i < n; ++i) {
    for (auto const n : graph[i]) {
      super_graph[group[i]].push_back(group[n]);
    }
  }
  ll num_disjoint = 0;
  std::vector<bool> visited(num_group);
  for (ll i = 0; i < num_group; ++i) {
    if (!visited[i]) {
      ++num_disjoint;
      dfs(i, super_graph, visited);
    }
  }
  std::cout << num_disjoint << std::endl;
  return num_disjoint == 1 && group[0] <= group[n - 1];
}

auto is_possible_by_degree(ll n, std::vector<std::pair<ll, ll>> const &edges) {
  std::vector<ll> indegree(n);
  std::vector<ll> outdegree(n);
  for (auto [a, b] : edges) {
    ++outdegree[a];
    ++indegree[b];
  }
  ll cnt = 0;
  for (ll i = 0; i < n; ++i) {
    cnt += indegree[i] != outdegree[i];
  }
  return cnt == 2 && ((outdegree[0] == indegree[0] + 1) &&
                      (indegree[n - 1] == outdegree[n - 1] + 1));
}

bool is_possible(ll n, std::vector<std::pair<ll, ll>> const &edges) {
  return /*is_connected(n, edges) &&*/ is_possible_by_degree(n, edges);
}

using graph_t = std::vector<std::unordered_set<ll>>;

void make_euler_path(ll i, graph_t &graph, std::vector<ll> &ans) {
  while (not std::empty(graph[i])) {
    auto const n = *graph[i].begin();
    graph[i].erase(n);
    make_euler_path(n, graph, ans);
  }
  ans.push_back(i);
}

auto solve(ll n, std::vector<std::pair<ll, ll>> const &edges) {
  if (!is_possible(n, edges)) {
    std::cout << "IMPOSSIBLE" << std::endl;
    return;
  }
  graph_t graph(n);
  for (auto [a, b] : edges) {
    graph[a].insert(b);
  }
  std::vector<ll> ans;
  make_euler_path(0, graph, ans);
  if (ans.size() != edges.size() + 1) {
    std::cout << "IMPOSSIBLE" << std::endl;
    return;
  }
  std::reverse(std::begin(ans), std::end(ans));
  for (auto n : ans)
    std::cout << n + 1 << ' ';
  std::cout << std::endl;
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  std::vector<std::pair<ll, ll>> edges(m);
  for (auto &[a, b] : edges) {
    a = read<ll>() - 1;
    b = read<ll>() - 1;
  }
  solve(n, edges);
}
