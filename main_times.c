#include <stdlib.h>
#include <stdio.h>
#include <sys/times.h>

#define WINDOW_SIZE 200000
#define MSG_COUNT 10000000

typedef char* msg;

typedef msg* channel;

clock_t worst_udelay = 0;
clock_t worst_sdelay = 0;

struct tms t0;
struct tms t1;

msg mk_msg(int n)
{
	return malloc(1024 * sizeof(char));
}

void push_msg(channel c, int high_id)
{
  times(&t0);

  // You can use this to push up the udelay
  // int x = 1;
  // for (int i = 0; i < 100000000; i++) { x *= 2; }

	size_t index = high_id % WINDOW_SIZE;

	if (WINDOW_SIZE <= high_id)
	{
		free(c[index]);
	}

	c[index] = mk_msg(high_id);

  times(&t1);

	clock_t udelay = t1.tms_utime - t0.tms_utime;
	if (worst_udelay < udelay) {
    worst_udelay = udelay;
    printf("New worst user delay: %lu\n", worst_udelay);
  }

	clock_t sdelay = t1.tms_stime - t0.tms_stime;
	if (worst_sdelay < sdelay) {
    worst_sdelay = sdelay;
    printf("New worst system delay: %lu\n", worst_sdelay);
  }
}

int main(void)
{
	channel c = calloc(WINDOW_SIZE, sizeof(msg));

	for (int i = 0; i < MSG_COUNT; i++)
	{
		push_msg(c, i);
	}

	printf("Worst user delay: %lu usec\n", worst_udelay);
	printf("Worst system delay: %lu usec\n", worst_sdelay);

	// TODO free up the channel and its messages! Not important here though.

	return 0;
}
