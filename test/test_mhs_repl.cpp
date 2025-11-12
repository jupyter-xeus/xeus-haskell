#include <boost/ut.hpp>
#include <xeus-haskell/mhs_repl.hpp>
using namespace boost::ut;
using namespace std::string_literals;

int main() {

  "positive repl test"_test = [] {
    xeus_haskell::MicroHsRepl repl;
    auto result = repl.execute("1 + 1");
    expect(eq(result.value(), "2\n"s));
  };


  "basic test"_test = [] {
    expect(42_i == 42);
    expect(1_i != 2);
  };

  "sum test"_test = [] {
    constexpr auto sum = [](auto... values) { return (values + ...); };
    expect(sum(1, 2) == 3_i);
    expect(sum(0) == 0_i);
  };
}
