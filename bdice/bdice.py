import random

# Tables
six_cutoffs = [ -1, -1, 3, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
eight_cutoffs = [ -1, -1, 4, 5, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7]
ten_cutoffs = [-1, -1, 5, 6, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9]
twelve_cutoffs = [ -1, -1, 6, 7, 8, 9, 9, 10, 10, 10, 10, 10, 10, 11, 11, 11]

initial_die_state = [12, 1, 1, 1]

def argmin(l):
    index, min_val = -1, 1000
    for i in range(len(l)):
        if l[i] < min_val:
            index, min_val = i, l[i]
    return index

def trymax(l):
    if (len(l) == 0):
        return -100000
    else:
        return max(l)
    
def generate_rolls(die_state, verbose=False):
    dice_left = sum(die_state)
    rolls = [[random.randint(1,6) for i in range(1,die_state[0] + 1)],
              [random.randint(1,8) for i in range(1,die_state[1] + 1)],
              [random.randint(1,10) for i in range(1,die_state[2] + 1)],
              [random.randint(1,12) for i in range(1,die_state[3] + 1)]]
    
    verbose and print(rolls)

    good_rolls = [ list(filter(lambda x: x > six_cutoffs[dice_left], rolls[0])),
                   list(filter(lambda x: x > eight_cutoffs[dice_left], rolls[1])),
                   list(filter(lambda x: x > ten_cutoffs[dice_left], rolls[2])),
                   list(filter(lambda x: x > twelve_cutoffs[dice_left], rolls[3]))]

    verbose and print(good_rolls)
    if (good_rolls != [[], [], [], []]):
        six_points = 6 * len(good_rolls[0])  - sum(good_rolls[0])
        six_removed = len(good_rolls[0])

        eight_points = 8 * len(good_rolls[1])  - sum(good_rolls[1])
        eight_removed = len(good_rolls[1])

        ten_points = 10 * len(good_rolls[2])  - sum(good_rolls[2])
        ten_removed = len(good_rolls[2])

        twelve_points = 12 * len(good_rolls[3])  - sum(good_rolls[3])
        twelve_removed = len(good_rolls[3])

        points = six_points + eight_points + ten_points + twelve_points
        die_state = [die_state[0] - six_removed, die_state[1] - eight_removed, die_state[2] - ten_removed, die_state[3] - twelve_removed]

        return [points, die_state, 0]
    else:
        # -100 is a bad hack to give a max on an empty set
        best_rolls = [six_cutoffs[dice_left] - trymax(rolls[0]) , 
                      eight_cutoffs[dice_left] - trymax(rolls[1]),
                      ten_cutoffs[dice_left] - trymax(rolls[2]), 
                      twelve_cutoffs[dice_left] - trymax(rolls[3])]
        verbose and print ("Best rolls")
        verbose and print(best_rolls)
        removable_die_pos = argmin(best_rolls)
        points = [6,8,10,12][removable_die_pos] - trymax(rolls[removable_die_pos])
        
        die_state[removable_die_pos] = die_state[removable_die_pos] - 1

        verbose and print("Removing die at pos %s " % (removable_die_pos))
        return [points, die_state, 1]

    
def run_sim():
    die_state = [12, 1, 1, 1]
    points = 0
    interruptions = 0
    while die_state != [0, 0, 0, 0]:
        result_vec = generate_rolls(die_state, verbose=False)
        points = points + result_vec[0]
        die_state = result_vec[1]
        interruptions = interruptions + result_vec[2]
    return [points, interruptions]

def averages(v):
    return [
        sum(list(map(lambda x: x[0], v))) / len(v),
        sum(list(map(lambda x: x[1], v))) / len(v)]

     

# Stats for the paper
# runs = [bdice.run_sim() for i in range(1, 1000001)]
# points = list(map(lambda x: x[0], runs))

# # Discards last chunk if too small
# def chop_into_chunks_of_n(my_list, n):
#     limit = int(len(my_list) / n)
#     return [my_list[i * n:(i + 1) * n] for i in range(limit)]

# chunks = chop_into_chunks_of_n(points, 6)

# def print_chunk_stats(points_arr, n):
#     random.shuffle(points_arr)
#     chunks = chop_into_chunks_of_n(points_arr, n)
#     mins = list(map(min, chunks))
#     mins.sort()
#     mins_grouped = [(k, list(g)) for k, g in itertools.groupby(mins)]
#     mins_histogram = list(map(lambda x: (x[0], len(x[1])), mins_grouped))
#     print("Stats for n = %s" % n)
#     print("Average: %s" % (sum(mins) / len(chunks)))
#     print("COL 1")
#     for h in mins_histogram:
#         print(h[0])
#     print("COL 2")
#     for h in mins_histogram:
#         print(h[1])


