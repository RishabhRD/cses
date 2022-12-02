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
    graph[b].push_back(a);
  }
  return graph;
}

auto solve(std::vector<std::pair<ll, ll>> const &edges, ll n) {
  auto const graph = build_graph(edges, n);
  std::vector<ll> from(n + 1, -1ll);
  std::queue<ll> q;
  q.push(n);
  ll ans = 0;
  [&] {
    while (not std::empty(q)) {
      ++ans;
      auto sz = std::size(q);
      while (sz--) {
        auto const top = q.front();
        if (top == 1) {
          return;
        }
        q.pop();
        for (auto n : graph[top]) {
          if (from[n] == -1) {
            q.push(n);
            from[n] = top;
          }
        }
      }
    }
  }();
  if (from[1] == -1) {
    std::cout << "IMPOSSIBLE" << std::endl;
  } else {
    std::cout << ans << std::endl;
    ll prev = 1;
    ll cur = 1;
    while (cur != n) {
      std::cout << cur << ' ';
      prev = cur;
      cur = from[cur];
    }
    std::cout << cur << std::endl;
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
