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

void dfs(std::vector<std::string> const &graph, ll i, ll j,
         std::vector<std::vector<bool>> &visited) {
  constexpr static ll dx[] = {0, 1, -1, 0};
  constexpr static ll dy[] = {1, 0, 0, -1};
  visited[i][j] = true;
  for (ll k = 0; k < 4; ++k) {
    auto const nx = i + dx[k];
    auto const ny = j + dy[k];
    if (nx >= 0 and ny >= 0 and nx < std::size(graph) and
        ny < std::size(graph[0]) and graph[nx][ny] == '.' &&
        visited[nx][ny] == false) {
      dfs(graph, nx, ny, visited);
    }
  }
}

auto solve(std::vector<std::string> const &graph) {
  auto const m = std::size(graph);
  auto const n = std::size(graph[0]);
  ll ans = 0;
  std::vector visited(m, std::vector(n, false));
  for (ll i = 0; i < m; ++i) {
    for (ll j = 0; j < n; ++j) {
      if (!visited[i][j] and graph[i][j] == '.') {
        dfs(graph, i, j, visited);
        ++ans;
      }
    }
  }
  return ans;
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  auto const graph = read_vec<std::string>(n);
  std::cout << solve(graph) << std::endl;
}
