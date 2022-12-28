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

void dfs(std::vector<std::vector<ll>> const &tree, ll i, ll prev,
         std::vector<ll> &parent, ll cur_len, std::vector<ll> &len) {
  len[i] = cur_len;
  parent[i] = prev;
  for (auto n : tree[i]) {
    if (n != prev) {
      dfs(tree, n, i, parent, cur_len + 1, len);
    }
  }
}

auto preprocess(std::vector<std::vector<ll>> const &tree) {
  auto const n = std::size(tree) - 1;
  std::vector dp(18, std::vector(n + 1, 0ll));
  std::vector<ll> len(n + 1);
  dfs(tree, 1, 1, dp[0], 0, len);
  for (ll i = 1; i < 18; ++i) {
    for (ll j = 0; j <= n; ++j) {
      dp[i][j] = dp[i - 1][dp[i - 1][j]];
    }
  }
  return std::pair{dp, len};
}

auto kth_ancestor(std::vector<std::vector<ll>> const &dp, ll i, ll k) {
  ll ans = i;
  std::bitset<18> bs(k);
  for (ll i = 0; i < 18; ++i) {
    if (bs[i]) {
      ans = dp[i][ans];
    }
  }
  return ans;
}

auto answer_query(std::vector<std::vector<ll>> const &dp,
                  std::vector<ll> const &len, ll a, ll b) {
  if (len[a] > len[b])
    std::swap(a, b);
  auto const diff = len[b] - len[a];
  b = kth_ancestor(dp, b, diff);
  if (a == b)
    return diff;
  ll sum = diff;
  for (ll i = 17; i >= 0; --i) {
    if (dp[i][a] != dp[i][b]) {
      a = dp[i][a];
      b = dp[i][b];
      sum += 2 * (1 << i);
    }
  }
  return sum + 2;
}

auto solve(ll n, std::vector<std::vector<ll>> const &tree,
           std::vector<std::pair<ll, ll>> const &queries) {
  auto const [dp, len] = preprocess(tree);
  for (auto [a, b] : queries) {
    auto const ans = answer_query(dp, len, a, b);
    std::cout << ans << std::endl;
  }
}

int main() {
  auto const n = read<ll>();
  auto const q = read<ll>();
  std::vector<std::vector<ll>> tree(n + 1);
  for (ll i = 0; i < n - 1; ++i) {
    auto const a = read<ll>();
    auto const b = read<ll>();
    tree[a].push_back(b);
    tree[b].push_back(a);
  }
  std::vector<std::pair<ll, ll>> queries;
  for (ll i = 0; i < q; ++i) {
    auto const a = read<ll>();
    auto const b = read<ll>();
    queries.push_back({a, b});
  }
  solve(n, tree, queries);
}
