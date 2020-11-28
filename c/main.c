#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>

#define WINDOW_SIZE 200000
#define MSG_COUNT 10000000

typedef char* msg;

typedef msg* channel;

struct timeval worst_delay = { 0, 0 };

struct timeval now()
{
    struct timeval t;
    gettimeofday(&t,NULL);
    return t;
}

msg mk_msg(int n)
{
	return malloc(1024 * sizeof(char));
}

void push_msg(channel c, int high_id)
{
	struct timeval t0 = now();

	size_t index = high_id % WINDOW_SIZE;

	if (WINDOW_SIZE <= high_id)
	{
		free(c[index]);
	}

	c[index] = mk_msg(high_id);

	struct timeval t1 = now();

	struct timeval delay;
	timersub(&t1, &t0, &delay);

	if (timercmp(&worst_delay, &delay, <))
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

	printf("Worst push time: %ld sec, %d usec", worst_delay.tv_sec, worst_delay.tv_usec);

	// TODO free up the channel and its messages! Not important here though.

	return 0;
}
