#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define WINDOW_SIZE 200000
#define MSG_COUNT 10000000

typedef char* msg;

typedef msg* channel;

clock_t worst_delay = 0;

clock_t now()
{
    return clock();
}

msg mk_msg(int n)
{
	return malloc(1024 * sizeof(char));
}

void push_msg(channel c, int high_id)
{
	clock_t t0 = now();

	size_t index = high_id % WINDOW_SIZE;

	if (WINDOW_SIZE <= high_id)
	{
		free(c[index]);
	}

	c[index] = mk_msg(high_id);

	clock_t t1 = now();

	clock_t delay = t1 - t0;

	if (worst_delay < delay)
	{
		worst_delay = delay;
	}
}

int main(void)
{
	channel c = calloc(WINDOW_SIZE, sizeof(msg));

	for (int i = 0; i < MSG_COUNT; i++)
	{
		push_msg(c, i);
	}

	printf("Worst push time: %lu usec", worst_delay);

	// TODO free up the channel and its messages! Not important here though.

	return 0;
}
