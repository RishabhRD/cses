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

template <typename T, typename GroupFunc> class segment_tree {
  GroupFunc func;
  ll n;
  std::vector<T> arr;

public:
  segment_tree(std::vector<T> const &arr, GroupFunc func)
      : func(std::move(func)), n(std::size(arr)), arr(n * 4) {
    build(1, 0, n - 1, arr);
  }

  T query(ll tl, ll tr) { return query(1, 0, n - 1, tl, tr); }

  void update(ll i, T const &val) {
    update(i, [val](auto e) { return val; });
  }

  template <typename F> void update(ll i, F &&f) { update(1, 0, n - 1, i, f); }

private:
  void build(ll i, ll l, ll r, std::vector<T> const &vec) {
    if (l == r) {
      arr[i] = vec[l];
    } else {
      ll const m = l + ((r - l) / 2);
      build(2 * i, l, m, vec);
      build(2 * i + 1, m + 1, r, vec);
      arr[i] = func(arr[2 * i], arr[2 * i + 1]);
    }
  }

  T query(ll i, ll l, ll r, ll tl, ll tr) {
    if (l == tl && r == tr)
      return arr[i];
    ll const m = l + ((r - l) / 2);
    if (tl >= l and tr <= m) {
      return query(2 * i, l, m, tl, tr);
    } else if (tl >= (m + 1) and tr <= r) {
      return query(2 * i + 1, m + 1, r, tl, tr);
    } else {
      auto const left = query(2 * i, l, m, tl, m);
      auto const right = query(2 * i + 1, m + 1, r, m + 1, tr);
      return func(left, right);
    }
  }

  template <typename F> void update(ll i, ll l, ll r, ll idx, F &&f) {
    if (l == r) {
      arr[i] = f(arr[i]);
      return;
    }
    ll const m = l + ((r - l) / 2);
    if (idx <= m) {
      update(2 * i, l, m, idx, f);
    } else {
      update(2 * i + 1, m + 1, r, idx, f);
    }
    arr[i] = func(arr[2 * i], arr[2 * i + 1]);
  }
};

template <typename T, typename GroupFunc>
segment_tree(std::vector<T> const &, GroupFunc) -> segment_tree<T, GroupFunc>;

auto const build_tree(ll n, std::vector<std::pair<ll, ll>> const &edges) {
  std::vector<std::vector<ll>> graph(n + 1);
  for (auto [a, b] : edges) {
    graph[a].push_back(b);
    graph[b].push_back(a);
  }
  return graph;
}

ll dfs(ll i, ll parent, ll cur_sum, std::vector<std::vector<ll>> const &tree,
       std::vector<ll> const &vals, std::vector<ll> &subtree_size,
       std::vector<ll> &idx, std::vector<ll> &dfs_tree) {
  idx[i] = dfs_tree.size();
  dfs_tree.push_back(vals[i] + cur_sum);
  subtree_size[i] = 1;
  for (auto n : tree[i]) {
    if (n != parent) {
      subtree_size[i] +=
          dfs(n, i, vals[i] + cur_sum, tree, vals, subtree_size, idx, dfs_tree);
    }
  }
  return subtree_size[i];
}

auto build_difference(std::vector<ll> const &arr) {
  auto diff = arr;
  for (ll i = 1; i < std::size(arr); ++i) {
    diff[i] = arr[i] - arr[i - 1];
  }
  return diff;
}

auto solve(std::vector<ll> &vals, std::vector<std::pair<ll, ll>> const &edges,
           std::vector<std::vector<ll>> const &queries) {
  ll const n = std::size(vals) - 1;
  auto const tree = build_tree(n, edges);
  std::vector<ll> subtree_size(n + 1);
  std::vector<ll> idx(n + 1);
  std::vector<ll> dfs_tree;
  dfs(1, 1, 0, tree, vals, subtree_size, idx, dfs_tree);
  segment_tree stree(build_difference(dfs_tree), std::plus<ll>{});
  for (auto const &query : queries) {
    if (query[0] == 1) {
      ll const s = query[1];
      ll const x = query[2];
      ll diff = x - vals[s];
      vals[s] = x;
      stree.update(idx[s], [&diff, x](auto e) { return e + diff; });
      if (idx[s] + subtree_size[s] < n) {
        stree.update(idx[s] + subtree_size[s],
                     [diff](ll e) { return e - diff; });
      }
    } else {
      ll const s = query[1];
      std::cout << stree.query(0, idx[s]) << std::endl;
    }
  }
}

int main() {
  auto const n = read<ll>();
  auto const q = read<ll>();
  std::vector<ll> vals(n + 1, 0);
  for (ll i = 1; i <= n; ++i)
    std::cin >> vals[i];
  std::vector<std::pair<ll, ll>> edges(n - 1);
  for (auto &[a, b] : edges)
    std::cin >> a >> b;
  std::vector<std::vector<ll>> queries;
  for (ll _ = 0; _ < q; ++_) {
    auto const q = read<ll>();
    if (q == 1) {
      auto const s = read<ll>();
      auto const x = read<ll>();
      queries.push_back(std::vector<ll>({q, s, x}));
    } else {
      auto const s = read<ll>();
      queries.push_back(std::vector<ll>({q, s}));
    }
  }
  solve(vals, edges, queries);
}
