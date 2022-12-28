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

auto preprocess(std::vector<ll> const &parent) {
  ll const n = std::size(parent);
  std::vector dp(25, std::vector(n, 0ll));
  dp[0] = parent;
  for (ll i = 1; i < 25; ++i) {
    for (ll j = 0; j < n; ++j) {
      dp[i][j] = dp[i - 1][dp[i - 1][j]];
    }
  }
  return dp;
}

auto kth_ancestor(std::vector<std::vector<ll>> const &dp, ll i, ll k) {
  ll ans = i;
  std::bitset<25> bs(k);
  for (ll i = 0; i < 25; ++i) {
    if (bs[i]) {
      ans = dp[i][ans];
    }
  }
  return ans;
}

auto get_lca(std::vector<std::vector<ll>> const &dp, std::vector<ll> const &len,
             ll a, ll b) {
  if (len[a] > len[b])
    std::swap(a, b);
  auto const diff = len[b] - len[a];
  b = kth_ancestor(dp, b, diff);
  if (a == b)
    return a;
  for (ll i = 24; i >= 0; --i) {
    if (dp[i][a] != dp[i][b]) {
      a = dp[i][a];
      b = dp[i][b];
    }
  }
  return dp[0][a];
}

auto build_tree(ll n, std::vector<std::pair<ll, ll>> const &edges) {
  std::vector<std::vector<ll>> graph(n + 1);
  for (auto [a, b] : edges) {
    graph[a].push_back(b);
    graph[b].push_back(a);
  }
  return graph;
}

void make_length(ll i, ll parent, ll cur,
                 std::vector<std::vector<ll>> const &tree,
                 std::vector<ll> &len) {
  len[i] = cur;
  for (auto n : tree[i]) {
    if (n != parent) {
      make_length(n, i, cur + 1, tree, len);
    }
  }
}

void make_parent(ll i, ll parent, std::vector<std::vector<ll>> const &tree,
                 std::vector<ll> &dp) {
  dp[i] = parent;
  for (auto n : tree[i]) {
    if (n != parent) {
      make_parent(n, i, tree, dp);
    }
  }
}

ll dfs(ll i, ll parent, std::vector<std::vector<ll>> const &tree,
       std::vector<ll> const &val, std::vector<ll> &res) {
  res[i] += val[i];
  for (auto n : tree[i]) {
    if (n != parent) {
      res[i] += dfs(n, i, tree, val, res);
    }
  }
  return res[i];
}

auto solve(ll n, std::vector<std::vector<ll>> const &tree,
           std::vector<std::pair<ll, ll>> const &paths) {
  std::vector<ll> len(n + 1);
  make_length(1, -1, 0, tree, len);
  std::vector<ll> parent(n + 1);
  make_parent(1, -1, tree, parent);
  auto const dp = preprocess(parent);
  std::vector<ll> val(n + 1, 0);
  for (auto const &[a, b] : paths) {
    auto const lca = get_lca(dp, len, a, b);
    ++val[a];
    ++val[b];
    --val[lca];
    if (parent[lca] != -1) {
      --val[parent[lca]];
    }
  }
  std::vector<ll> res(n + 1);
  dfs(1, -1, tree, val, res);
  for (ll i = 1; i <= n; ++i) {
    std::cout << res[i] << ' ';
  }
  std::cout << std::endl;
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  std::vector<std::vector<ll>> graph(n + 1);
  for (ll i = 0; i < n - 1; ++i) {
    auto const a = read<ll>();
    auto const b = read<ll>();
    graph[a].push_back(b);
    graph[b].push_back(a);
  }
  std::vector<std::pair<ll, ll>> paths;
  for (ll i = 0; i < m; ++i) {
    auto const a = read<ll>();
    auto const b = read<ll>();
    paths.push_back({a, b});
  }
  solve(n, graph, paths);
}
