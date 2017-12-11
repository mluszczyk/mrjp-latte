#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

void printInt(int num) {
  printf("%d\n", num);
}

void printString(const char* str) {
  puts(str);
}

char* concat(const char* str1, const char* str2) {
  char* res = malloc(sizeof(char) * (strlen(str1) + strlen(str2) + 1));
  strcpy(res, str1);
  strcat(res, str2);
  return res;
}

bool streq(const char* str1, const char* str2) {
  return (strcmp(str1, str2) == 0);
}

bool strne(const char* str1, const char* str2) {
  return !streq(str1, str2);
}

const char* readString() {
  char *line;
  size_t num;
  size_t res = getline(&line, &num, stdin);

  if (res == -1) {
    return "";
  }
  if (res > 0 && line[res - 1] == '\n') {
    line[res - 1] = '\0';
  }
  return line;
}

int readInt() {
  int num;
  scanf("%d", &num);
  return num;
}

void error() {
  fprintf(stderr, "runtime error\n");
  exit(1);
}
