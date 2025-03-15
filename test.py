import subprocess
import os

test_dir = "free-foil-sprite/test/code"
pos_dir = os.path.join(test_dir, "pos")
neg_dir = os.path.join(test_dir, "neg")
executable = ["cabal", "run", "free-foil-sprite-exe", "--"]

TESTS_TO_IGNORE = [
    "const00.re",
    "id01.re",
    "id00.re"
]

def run_test(test_path, expected):
    result = subprocess.run(executable + [test_path], capture_output=True, text=True)
    output = "\n".join(result.stdout.strip().split("\n")[-10:])  # Get last line of output

    if expected in output:
        print(f"✅: {test_path}")
    else:
        print(f"❌: {test_path} (expected '{expected}', got '{output}')")

# Run positive tests
print("Running positive tests...")
for test_file in os.listdir(pos_dir):
    if test_file in TESTS_TO_IGNORE:
        continue
    test_path = os.path.join(pos_dir, test_file)
    if os.path.isfile(test_path):
        run_test(test_path, "Safe")


# Run negative tests
print("\nRunning negative tests...")
for test_file in os.listdir(neg_dir):
    if test_file in TESTS_TO_IGNORE:
        continue
    test_path = os.path.join(neg_dir, test_file)
    if os.path.isfile(test_path):
        run_test(test_path, "Unsafe")
