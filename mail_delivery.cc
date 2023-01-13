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

bool is_connected(ll n, std::vector<std::pair<ll, ll>> const &edges) {
  dsu dsu(n);
  for (auto [a, b] : edges) {
    dsu.combine(a, b);
  }
  safe_set group;
  for (auto [a, b] : edges) {
    group.insert(dsu.find(a));
    group.insert(dsu.find(b));
  }
  return group.size() == 1 && group.count(dsu.find(0));
}

bool is_possible_with_degree(ll n,
                             std::vector<std::pair<ll, ll>> const &edges) {
  std::vector<ll> degree(n);
  for (auto [a, b] : edges) {
    ++degree[a];
    ++degree[b];
  }
  ll const num_odd = std::count_if(std::begin(degree), std::end(degree),
                                   [](auto n) { return n % 2; });
  return num_odd == 0 /*|| (num_odd == 2 && (degree[0] % 2 == 1))*/;
}

bool is_possible(ll n, std::vector<std::pair<ll, ll>> const &edges) {
  return is_connected(n, edges) && is_possible_with_degree(n, edges);
}

using graph_t = std::vector<safe_set>;

void make_euler_path(ll i, graph_t &graph, std::vector<ll> &ans) {
  while (graph[i].size() != 0) {
    auto const n = *graph[i].begin();
    graph[i].erase(n);
    graph[n].erase(i);
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
    graph[b].insert(a);
  }
  std::vector<ll> ans;
  make_euler_path(0, graph, ans);
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
