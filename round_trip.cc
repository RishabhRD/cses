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

class dsu {
private:
  using ll = long long;
  std::vector<ll> parent_;
  std::vector<ll> size_;

public:
  dsu(ll n) : parent_(n), size_(n, 1) {
    std::iota(std::begin(parent_), std::end(parent_), 0);
  }

  ll find(ll n) {
    if (parent_[n] == n)
      return n;
    return parent_[n] = find(parent_[n]);
  }

  void combine(ll x, ll y) {
    auto const px = find(x);
    auto const py = find(y);
    if (px == py)
      return;
    if (size_[px] >= size_[py]) {
      size_[px] += size_[py];
      parent_[py] = px;
    } else {
      size_[py] += size_[px];
      parent_[px] = py;
    }
  }

  auto size(ll n) { return size_[find(n)]; }
};

using graph_t = std::vector<std::vector<ll>>;

auto build_graph(std::vector<std::pair<ll, ll>> const &edges, ll n) {
  graph_t graph(n + 1);
  for (auto [a, b] : edges) {
    graph[a].push_back(b);
    graph[b].push_back(a);
  }
  return graph;
}

void dfs(graph_t const &graph, ll i, ll init, ll end, std::vector<ll> &cur,
         std::vector<ll> &res, std::vector<bool> &visited) {
  if (visited[i]) {
    return;
  }
  visited[i] = true;
  cur.push_back(i);
  if (i == end)
    res = cur;
  for (auto n : graph[i]) {
    if (!(i == init && n == end)) {
      dfs(graph, n, init, end, cur, res, visited);
    }
  }
  cur.pop_back();
}

auto solve(std::vector<std::pair<ll, ll>> const &edges, ll n) {
  auto const graph = build_graph(edges, n);
  dsu dsu(n + 1);
  auto const op_ends = [&]() -> std::optional<std::pair<ll, ll>> {
    for (auto [a, b] : edges) {
      if (dsu.find(a) == dsu.find(b))
        return std::pair{a, b};
      dsu.combine(a, b);
    }
    return std::nullopt;
  }();
  if (!op_ends.has_value()) {
    std::cout << "IMPOSSIBLE" << std::endl;
  } else {
    auto const [init, end] = op_ends.value();
    std::vector<ll> res;
    std::vector<ll> cur;
    std::vector visited(n + 1, false);
    dfs(graph, init, init, end, cur, res, visited);
    std::cout << res.size() + 1 << std::endl;
    for (auto ele : res)
      std::cout << ele << ' ';
    std::cout << init << std::endl;
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
