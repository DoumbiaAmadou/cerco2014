int tab1[5] = { 10, -3, 25, 56, -32, };

void copy(int *dst, int *src, int size)
{
  int i;
  _cost2:
  for (i = 0; i < size; i = i + 1) {
    _cost0:
    dst[i] = src[i];
  }
  _cost1:
  /*skip*/;
}

void print_tab(int *tab, int size)
{
  int i;
  _cost5:
  for (i = 0; i < size; i = i + 1) {
    _cost3:
    print_sint(tab[i]);
    space();
  }
  _cost4:
  /*skip*/;
  newline();
}

int main(void)
{
  int tab2[5];
  int tab3[5];
  _cost6:
  tab3[0] = 0;
  tab3[1] = 1;
  tab3[2] = 2;
  tab3[3] = 3;
  tab3[4] = 4;
  copy(tab2, tab1, 5);
  copy(tab1, tab3, 5);
  print_tab(tab1, 5);
  print_tab(tab2, 5);
  print_tab(tab3, 5);
  return 0;
}


