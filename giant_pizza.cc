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

auto read_with_sign() {
  auto const sign = read<char>();
  auto const b = read<ll>();
  if (sign == '+') {
    return b;
  } else {
    return -b;
  }
}

using graph_t = std::unordered_map<ll, std::vector<ll>>;

void dfs1(ll i, graph_t const &graph, std::unordered_set<ll> &visited,
          std::vector<ll> &order) {
  if (visited.count(i))
    return;
  visited.insert(i);
  for (ll n : graph.at(i)) {
    dfs1(n, graph, visited, order);
  }
  order.push_back(i);
}

void dfs2(ll i, graph_t const &graph, ll group,
          std::unordered_map<ll, ll> &connect_mp) {
  if (connect_mp[i] != 0) {
    return;
  }
  connect_mp[i] = group;
  for (ll n : graph.at(i)) {
    dfs2(n, graph, group, connect_mp);
  }
}

auto solve(ll m, graph_t const &graph, graph_t const &op_graph) {
  std::vector<ll> order;
  {
    std::unordered_set<ll> visited;
    for (auto const &[k, _] : graph) {
      dfs1(k, graph, visited, order);
    }
  }
  std::reverse(std::begin(order), std::end(order));
  std::unordered_map<ll, ll> connect_map;
  {
    ll num_group = 0;
    for (auto const n : order) {
      if (connect_map[n] == 0) {
        ++num_group;
        dfs2(n, op_graph, num_group, connect_map);
      }
    }
  }
  std::vector<ll> assignment(m + 1);
  for (ll i = 1; i <= m; ++i) {
    if (connect_map[i] == connect_map[-i]) {
      std::cout << "IMPOSSIBLE" << std::endl;
      return;
    }
    assignment[i] = connect_map[i] > connect_map[-i];
  }
  for (ll i = 1; i <= m; ++i) {
    if (assignment[i] == false)
      std::cout << "- ";
    else {
      std::cout << "+ ";
    }
  }
  std::cout << std::endl;
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  std::unordered_map<ll, std::vector<ll>> graph;
  std::unordered_map<ll, std::vector<ll>> op_graph;
  for (ll i = 1; i <= m; ++i) {
    graph[i];
    graph[-i];
    op_graph[i];
    op_graph[-i];
  }
  for (ll i = 0; i < n; ++i) {
    auto const a = read_with_sign();
    auto const b = read_with_sign();
    graph[-a].push_back(b);
    graph[-b].push_back(a);
    op_graph[a].push_back(-b);
    op_graph[b].push_back(-a);
  }
  solve(m, graph, op_graph);
}
