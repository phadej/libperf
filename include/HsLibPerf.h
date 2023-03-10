#ifndef HS_LIBPERF_H
#define HS_LIBPERF_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <unistd.h>
#include <linux/perf_event.h>
#include <sys/capability.h>
#include <sys/syscall.h>
#include <sys/ioctl.h>
#include <errno.h>

enum HsPerfCounter {
  /* hw counters */
  HS_PERF_COUNT_HW_CPU_CYCLES,
  HS_PERF_COUNT_HW_INSTRUCTIONS,
  HS_PERF_COUNT_HW_CACHE_REFERENCES,
  HS_PERF_COUNT_HW_CACHE_MISSES,
  HS_PERF_COUNT_HW_BRANCH_INSTRUCTIONS,
  HS_PERF_COUNT_HW_BRANCH_MISSES,
  HS_PERF_COUNT_HW_REF_CPU_CYCLES,
  HS_PERF_COUNT_SW_PAGE_FAULTS,
  HS_PERF_COUNT_SW_PAGE_FAULTS_MIN,
  HS_PERF_COUNT_SW_PAGE_FAULTS_MAJ,
  HS_PERF_COUNT_SW_DUMMY
};


static inline int HsPerfEventOpen(struct perf_event_attr *attr, pid_t pid, int cpu, int group_fd, unsigned long flags) {
  return syscall(SYS_perf_event_open, attr, pid, cpu, group_fd, flags);
}

static inline int HsLibPerfOpen(enum HsPerfCounter c, int group_fd, int format_group) {
  unsigned long flags = 0;

  struct perf_event_attr pe = {0};

  pe.size = sizeof(struct perf_event_attr);
  pe.disabled = 1;
  pe.exclude_hv = 1;
  /* if we exclude kernel events, we don't need additional priveleges */
  /* pe.exclude_kernel = 1; */

  if (format_group) {
    pe.read_format = PERF_FORMAT_GROUP;
  }

  switch (c) {
    case HS_PERF_COUNT_HW_CPU_CYCLES:
    pe.type = PERF_TYPE_HARDWARE;
    pe.config = PERF_COUNT_HW_CPU_CYCLES;
    break;

  case HS_PERF_COUNT_HW_INSTRUCTIONS:
    pe.type = PERF_TYPE_HARDWARE;
    pe.config = PERF_COUNT_HW_INSTRUCTIONS;
    break;

  case HS_PERF_COUNT_HW_CACHE_REFERENCES:
    pe.type = PERF_TYPE_HARDWARE;
    pe.config = PERF_COUNT_HW_CACHE_REFERENCES;
    break;

  case HS_PERF_COUNT_HW_CACHE_MISSES:
    pe.type = PERF_TYPE_HARDWARE;
    pe.config = PERF_COUNT_HW_CACHE_MISSES;
    break;

  case HS_PERF_COUNT_HW_BRANCH_INSTRUCTIONS:
    pe.type = PERF_TYPE_HARDWARE;
    pe.config = PERF_COUNT_HW_BRANCH_INSTRUCTIONS;
    break;

  case HS_PERF_COUNT_HW_BRANCH_MISSES:
    pe.type = PERF_TYPE_HARDWARE;
    pe.config = PERF_COUNT_HW_BRANCH_MISSES;
    break;

  case HS_PERF_COUNT_HW_REF_CPU_CYCLES:
    pe.type = PERF_TYPE_HARDWARE;
    pe.config = PERF_COUNT_HW_REF_CPU_CYCLES;
    break;

  case HS_PERF_COUNT_SW_PAGE_FAULTS:
    pe.type = PERF_TYPE_SOFTWARE;
    pe.config = PERF_COUNT_SW_PAGE_FAULTS;
    break;

  case HS_PERF_COUNT_SW_PAGE_FAULTS_MIN:
    pe.type = PERF_TYPE_SOFTWARE;
    pe.config = PERF_COUNT_SW_PAGE_FAULTS_MIN;
    break;

  case HS_PERF_COUNT_SW_PAGE_FAULTS_MAJ:
    pe.type = PERF_TYPE_SOFTWARE;
    pe.config = PERF_COUNT_SW_PAGE_FAULTS_MAJ;
    break;

  case HS_PERF_COUNT_SW_DUMMY:
    pe.type = PERF_TYPE_SOFTWARE;
    pe.config = PERF_COUNT_SW_DUMMY;
    break;

  default:
    return EINVAL;
  }

  return HsPerfEventOpen(&pe, 0, -1, group_fd, flags);
};

static inline uint64_t HsLibPerfRead(int fd) {
  ssize_t res = 0;
  uint64_t count = 0;
  ssize_t r = read(fd, &count, sizeof(count)); /* we ignore errors here */
  return count;
}

static inline int HsLibPerfRawRead(int fd, int count, uint64_t *values) {
  return read(fd, values, sizeof(uint64_t) * count);
}

static inline int HsLibPerfEnable(int fd) {
  ioctl(fd, PERF_EVENT_IOC_ENABLE, PERF_IOC_FLAG_GROUP);
}

static inline int HsLibPerfDisable(int fd) {
  ioctl(fd, PERF_EVENT_IOC_DISABLE, PERF_IOC_FLAG_GROUP);
}

static inline int HsLibPerfReset(int fd) {
  ioctl(fd, PERF_EVENT_IOC_RESET, PERF_IOC_FLAG_GROUP);
}

static inline int HsLibPerfHasCapPerfMon() {
  int res;
  cap_value_t cap_p;

  res = cap_from_name("CAP_PERFMON", &cap_p);
  if (res == -1) {
      res = cap_from_name("CAP_SYS_ADMIN", &cap_p);
      if (res == -1) {
          return 0;
      }
  }

  cap_t cap = cap_get_proc();
  cap_flag_value_t v = 0;
  cap_get_flag(cap, cap_p, CAP_EFFECTIVE, &v);
  cap_free(cap);
  return v;
}

#ifdef __cplusplus
}
#endif
#endif /* HS_LIBPERF_H */
