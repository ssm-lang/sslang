#ifndef POSIX_COMMON_H
#define POSIX_COMMON_H

#include <ssm-internal.h>

#include <ctype.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdio.h>
#include <string.h>
#include <sys/select.h>
#include <time.h>
#include <unistd.h>

// #define DBG(...) fprintf(stderr, __VA_ARGS__)
#define DBG(...)                                                               \
  do {                                                                         \
  } while (0)

#define NANOS 1000000000L

#define timespec_diff(a, b)                                                    \
  (a).tv_nsec - (b).tv_nsec < 0                                                \
      ? (struct timespec){.tv_sec = (a).tv_sec - (b).tv_sec - 1,               \
                          .tv_nsec = (a).tv_nsec - (b).tv_nsec + NANOS}        \
      : (struct timespec) {                                                    \
    .tv_sec = (a).tv_sec - (b).tv_sec, .tv_nsec = (a).tv_nsec - (b).tv_nsec    \
  }

#define timespec_add(a, b)                                                     \
  (a).tv_nsec + (b).tv_nsec > NANOS                                            \
      ? (struct timespec){.tv_sec = (a).tv_sec + (b).tv_sec + 1,               \
                          .tv_nsec = (a).tv_nsec + (b).tv_nsec - NANOS}        \
      : (struct timespec) {                                                    \
    .tv_sec = (a).tv_sec + (b).tv_sec, .tv_nsec = (a).tv_nsec + (b).tv_nsec    \
  }

#define timespec_lt(a, b)                                                      \
  (a).tv_sec == (b).tv_sec ? (a).tv_nsec < (b).tv_nsec : (a).tv_sec < (b).tv_sec

#define timespec_time(t) (((t).tv_sec * NANOS) + (t).tv_nsec)

#define timespec_of(ns)                                                        \
  (struct timespec) { .tv_sec = (ns) / NANOS, .tv_nsec = (ns) % NANOS }

typedef int fd_sem_t[2];

#define fd_sem_write(fd_sem) write(fd_sem[1], "a", 1)
#define fd_sem_read(fd_sem)                                                    \
  do {                                                                         \
    char buf;                                                                  \
    read(fd_sem[0], &buf, 1);                                                  \
  } while (0)

extern int ssm_sem_fd[2];
extern atomic_size_t rb_r;
extern atomic_size_t rb_w;
extern pthread_mutex_t rb_lk;

#endif /* POSIX_COMMON_H */
