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

void dfs1(ll i, graph_t const &graph, std::vector<bool> &visited,
          std::vector<ll> &order) {
  if (visited[i])
    return;
  visited[i] = true;
  for (auto n : graph[i])
    dfs1(n, graph, visited, order);
  order.push_back(i);
}

void dfs2(ll i, graph_t const &graph, ll cur_group, std::vector<ll> &group) {
  if (group[i] != -1)
    return;
  group[i] = cur_group;
  for (auto n : graph[i])
    dfs2(n, graph, cur_group, group);
}

ll dfs3(ll i, ll parent, std::vector<std::vector<ll>> const &graph,
        std::vector<ll> const &coins, std::vector<ll> &dp) {
  if (dp[i] != -1)
    return dp[i];
  dp[i] = coins[i];
  for (auto n : graph[i]) {
    if (n != parent) {
      dp[i] = std::max(dp[i], coins[i] + dfs3(n, i, graph, coins, dp));
    }
  }
  return dp[i];
}

auto solve(graph_t const &graph, graph_t const &op_graph,
           std::vector<ll> const &coins) {
  auto const n = graph.size();
  std::vector<ll> order;
  {
    std::vector<bool> visited(n);
    for (ll i = 0; i < n; ++i) {
      dfs1(i, graph, visited, order);
    }
  }
  std::reverse(std::begin(order), std::end(order));
  std::vector<ll> group(n, -1);
  ll num_group = 0;
  for (auto n : order) {
    if (group[n] == -1) {
      dfs2(n, op_graph, ++num_group, group);
    }
  }
  std::vector<ll> group_coin(num_group + 1);
  for (ll i = 0; i < n; ++i) {
    group_coin[group[i]] += coins[i];
  }
  std::vector<std::vector<ll>> super_tree(num_group + 1);
  for (ll i = 0; i < n; ++i) {
    for (auto n : graph[i]) {
      if (group[i] != group[n]) {
        super_tree[group[i]].push_back(group[n]);
      }
    }
  }
  std::vector<ll> super_coins(num_group + 1, -1);
  for (ll i = 1; i <= num_group; ++i) {
    dfs3(i, i, super_tree, group_coin, super_coins);
  }
  std::cout << *std::max_element(std::begin(super_coins), std::end(super_coins))
            << std::endl;
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  auto const coins = read_vec<ll>(n);
  graph_t graph(n);
  graph_t op_graph(n);
  for (ll i = 0; i < m; ++i) {
    auto const x = read<ll>() - 1;
    auto const y = read<ll>() - 1;
    graph[x].push_back(y);
    op_graph[y].push_back(x);
  }
  solve(graph, op_graph, coins);
}
