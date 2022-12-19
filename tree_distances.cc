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

struct path_desc_t {
  ll next;
  ll len;
};

struct max_path_entry {
  path_desc_t first;
  path_desc_t second;
};

void print(path_desc_t desc) { std::cout << desc.len << ' ' << desc.next; }

void print(max_path_entry cur) {
  auto const first = cur.first;
  auto const second = cur.second;
  print(first);
  std::cout << "      ";
  print(second);
  std::cout << std::endl;
}

max_path_entry add_path(max_path_entry cur, path_desc_t path) {
  if (path.len >= cur.first.len) {
    if (path.next != cur.first.next) {
      cur.second = cur.first;
      cur.first = path;
    }
  } else if (path.len >= cur.second.len) {
    if (path.next != cur.second.next) {
      cur.second = path;
    }
  }
  return cur;
}

max_path_entry child_path(ll i, ll prev,
                          std::vector<std::vector<ll>> const &tree,
                          std::vector<max_path_entry> &dp) {
  max_path_entry entry{{i, 0}, {i, 0}};
  for (auto n : tree[i]) {
    if (n != prev) {
      auto [first, second] = child_path(n, i, tree, dp);
      first.next = n;
      second.next = n;
      ++first.len;
      ++second.len;
      entry = add_path(entry, first);
      if (second.next != first.next)
        entry = add_path(entry, second);
    }
  }
  return dp[i] = entry;
}

void make_parent(ll i, ll prev, std::vector<std::vector<ll>> const &tree,
                 std::vector<ll> &parent) {
  for (auto n : tree[i]) {
    if (n != prev) {
      parent[n] = i;
      make_parent(n, i, tree, parent);
    }
  }
}

void dfs(ll i, ll prev, std::vector<std::vector<ll>> const &tree,
         std::vector<ll> const &parent, std::vector<max_path_entry> &dp) {
  if (dp[parent[i]].first.next == i) {
    auto cur_path = dp[parent[i]].second;
    cur_path.next = parent[i];
    ++cur_path.len;
    dp[i] = add_path(dp[i], cur_path);
  } else {
    auto cur_path = dp[parent[i]].first;
    cur_path.next = parent[i];
    ++cur_path.len;
    dp[i] = add_path(dp[i], cur_path);
  }
  for (auto n : tree[i]) {
    if (n != prev) {
      dfs(n, i, tree, parent, dp);
    }
  }
}

auto solve(ll n, std::vector<std::pair<ll, ll>> const &edges) {
  std::vector<std::vector<ll>> tree(n);
  for (auto [a, b] : edges) {
    tree[a].push_back(b);
    tree[b].push_back(a);
  }
  std::vector<max_path_entry> dp(n);
  std::vector<ll> parent(n);
  make_parent(0, -1, tree, parent);
  child_path(0, -1, tree, dp);
  for (auto n : tree[0]) {
    dfs(n, 0, tree, parent, dp);
  }
  for (auto [first, _] : dp) {
    std::cout << first.len << ' ';
  }
  std::cout << std::endl;
}

int main() {
  auto const n = read<ll>();
  std::vector<std::pair<ll, ll>> edges;
  for (ll i = 0; i < n - 1; ++i) {
    auto const a = read<ll>() - 1;
    auto const b = read<ll>() - 1;
    edges.push_back({a, b});
  }
  solve(n, edges);
}
