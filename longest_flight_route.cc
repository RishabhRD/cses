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

ll max_path_len(graph_t const &graph, ll i, std::vector<ll> &len,
                std::vector<ll> &succesor) {
  if (len[i] != -1)
    return len[i];
  if (i == std::size(graph) - 1)
    return len[i] = 1;
  ll ans = 0;
  ll cur_successor = 0;
  for (auto const n : graph[i]) {
    auto const cur_len = max_path_len(graph, n, len, succesor);
    if (cur_len != 0 and cur_len > ans) {
      ans = cur_len;
      cur_successor = n;
    }
  }
  succesor[i] = cur_successor;
  if (ans == 0)
    return len[i] = ans;
  return len[i] = 1 + ans;
}

auto solve(std::vector<std::pair<ll, ll>> const &edges, ll n) {
  auto const graph = build_graph(edges, n);
  std::vector<ll> len(n + 1, -1);
  std::vector<ll> succesor(n + 1, 0);
  auto const ans = max_path_len(graph, 1, len, succesor);
  if (ans == 0) {
    std::cout << "IMPOSSIBLE" << std::endl;
  } else {
    ll node = 1;
    std::cout << ans << std::endl;
    while (node != 0) {
      std::cout << node << ' ';
      node = succesor[node];
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
