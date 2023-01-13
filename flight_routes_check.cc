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
          std::vector<ll> &order, std::vector<bool> &visited) {
  if (visited[i])
    return;
  visited[i] = true;
  for (auto n : graph[i])
    dfs1(n, graph, order, visited);
  order.push_back(i);
}

void dfs2(ll i, std::vector<std::vector<ll>> const &graph,
          std::vector<bool> &visited) {
  if (visited[i])
    return;
  visited[i] = true;
  for (auto n : graph[i])
    dfs2(n, graph, visited);
}

auto solve(ll n, std::vector<std::vector<ll>> const &graph,
           std::vector<std::vector<ll>> const &op_graph) {
  std::vector<ll> order;
  {
    std::vector<bool> visited(n);
    for (ll i = 0; i < n; ++i) {
      dfs1(i, graph, order, visited);
    }
  }
  std::reverse(std::begin(order), std::end(order));
  {
    std::vector<bool> visited(n);
    ll num_components = 0;
    ll prev_component = -1;
    for (ll n : order) {
      if (!visited[n]) {
        if (num_components == 1) {
          std::cout << "NO" << std::endl;
          std::cout << n + 1 << ' ' << prev_component + 1 << std::endl;
          return;
        } else {
          ++num_components;
          prev_component = n;
          dfs2(n, op_graph, visited);
        }
      }
    }
  }
  std::cout << "YES" << std::endl;
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  std::vector<std::vector<ll>> graph(n);
  std::vector<std::vector<ll>> op_graph(n);
  for (ll i = 0; i < m; ++i) {
    auto const a = read<ll>() - 1;
    auto const b = read<ll>() - 1;
    graph[a].push_back(b);
    op_graph[b].push_back(a);
  }
  solve(n, graph, op_graph);
}
