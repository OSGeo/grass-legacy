/* lia/stack.c  ---  A global stack of Lia objects. */

/*--------------------------------------------------------------------------*/

#include "base.h"

/*--------------------------------------------------------------------------*/

typedef struct element_type
{
  Lia_ptr lia_object;
  struct element_type *next;
} Element;

typedef struct stack_type
{
  Element *top, *free;
} Stack;

/*--------------------------------------------------------------------------*/

static Stack s;          /* This is the global but hidden Lia stack. */
static int size = 0;
static int limit = 100;  /* Procedure lia_push() will produce an error
                            if stack size > limit.  (Emergency break!) */

static void lia_binop();

/*--------------------------------------------------------------------------*/

void lia_stack_limit (new_limit)
     int new_limit;
     /* Allows the user to change the upper bound of the stack size.
        This limit is artificial and just acts like an emergenc break! */
{
  limit = new_limit;
}

/*--------------------------------------------------------------------------*/

int lia_stack_empty ()
     /* Returns TRUE iff Lia stack is empty. */
{
  return (s.top == NULL);
}

/*---------------------------------------------------------------------------*/

int lia_stack_size ()
     /* Returns the current number of elements on the Lia stack. */
{
  return (size);
}

/*--------------------------------------------------------------------------*/

void lia_push (longi)
     Lia_obj longi;
     /* Pushes longi on top of Lia stack by copying its contents.
        NOTE: - If longi == LIA_NULL, it pushes an empty lia_object.
              - It will produce error when stack size reaches upper limit. */
{
  Element *new_top;
  if (s.free)
    {
      new_top = s.free;
      s.free = s.free->next;
    }
  else if (size == limit)
    basic_error ("lia_push: reached upper limit (%d)", limit);
  else
    {
      new_top = MALLOC (Element, 1);
      MARK (new_top, -LIA_MAGIC);
      new_top->lia_object = MALLOC (Lia, lia_common.max);
      MARK (new_top->lia_object, -LIA_MAGIC);
      size ++;
    }
  new_top->next = s.top;
  s.top = new_top;
  if (longi)
    lia_assign (s.top->lia_object, longi);
}

/*---------------------------------------------------------------------------*/

Lia_ptr lia_pushf (longi)
     Lia_obj longi;
     /* Pushes longi on stack and returns a pointer to this element. */
{
  lia_push (longi);
  return (s.top->lia_object);
}

/*--------------------------------------------------------------------------*/

void lia_pushtop ()
     /* Puhes current top again on stack; ie, it "duplicates" it. */
{
  lia_push ((Lia_obj) NULL);
  if (not (s.top and s.top->next))
    basic_error ("lia_pushtop: underflow");
  lia_assign (s.top->lia_object, s.top->next->lia_object);  
}

/*--------------------------------------------------------------------------*/

void lia_pop (longi)
     Lia_obj longi;
     /* Pops top from the Lia stack and copies contents into longi.
        NOTE: It will pop WITHOUT copying when longi == LIA_NULL. */
     
{
  Element *old_top = s.top;
  if (not s.top)
    basic_error ("lia_pop: underflow");
  s.top = old_top->next;
  old_top->next = s.free;
  s.free = old_top;
  if (longi)
    lia_assign (longi, old_top->lia_object);
}

/*--------------------------------------------------------------------------*/

Lia_ptr lia_popf ()
     /* Returns pointer to the top of the Lia stack AND pops it.
        NOTE: The memory of the popped element will be re-used by the
              next push. It's the users responsibility to copy it via
              lia_assign()/lia_copy() if the Lia object is needed in
              future. */
{
  Lia_ptr t = s.top->lia_object;
  lia_pop (LIA_NULL);
  return (t);
}

/*--------------------------------------------------------------------------*/

void lia_times ()
     /* Stack multiplication:
        stack[..., top(-1), top]  -->  stack[..., top(-1) * top]. */
{
  lia_binop (lia_mul);
}

/*--------------------------------------------------------------------------*/

void lia_plus ()
     /* Stack addition:
        stack[..., top(-1), top]  -->  stack[..., top(-1) + top]. */
{
  lia_binop (lia_add);
}

/*--------------------------------------------------------------------------*/

void lia_minus ()
     /* Stack subtraction:
        stack[..., (top-1), top] --> stack[..., (top-1) - top]. */
{
  lia_binop (lia_sub);
}

/*--------------------------------------------------------------------------*/

void lia_power ()
     /* Stack power function:
        stack[..., top(-1), top] --> stack[..., top(-1) ^ top]. */
     /* NOTE: This works only for "small" and positive powers!
              Here, "small" means that the Lia object can be expressed
              as a normal int value, because this implmentation of
              lia_power() uses lia_ipower(). */
{
  if (not s.top)
    basic_error ("lia_power: underflow");
  if (s.top->lia_object[0] != 2)
    basic_error ("lia_power: not yet implemented for %s",
                 "large or negative powers");
  else
    { /* lenght(top) == 1 and sign(top) == positive */
      int power = (int) s.top->lia_object[1];
      lia_pop (LIA_NULL);
      lia_ipower (power);
    }
}

/*--------------------------------------------------------------------------*/

void lia_ipower (p)
     int p;
     /* Uses the Lia stack to compute top^p:
        stack[..., top] --> stack[..., top ^ p] with p >= 0. */
{
  if (p == 0)
    { /* (very) special case: stack[..., top] --> stack[..., 1] */
      if (not s.top)
        basic_error ("lia_ipower: undeflow");
      s.top->lia_object[1] = 1;  /* set value to 1 */
      s.top->lia_object[0] = 2;  /* set lenght to 1 and sign positive */
    }
  else if (p == 1)
    { /* (normal) bottom of recursion: stack[..., top] --> stack[..., top] */
      return;
    }
  else if (Odd (p))
    {
      lia_pushtop ();       /* --> stack[..., top, top]           */
      lia_ipower (p - 1);   /* --> stack[..., top, top ^ (p - 1)] */
      lia_binop (lia_mul);  /* --> stack[..., top ^ p]            */

    }
  else
    { /* make time complexity logarithmic in p by squaring:             */
      lia_ipower (p / 2);   /* --> stack[..., top ^ (p/2)]              */
      lia_pushtop ();       /* --> stack[..., top ^ (p/2), top ^ (p/2)] */
      lia_binop (lia_mul);  /* --> stack[..., top ^ p]                  */
    }
}

/*--------------------------------------------------------------------------*/

static void lia_binop (op)
     void (*op) ();
     /* Pops top & top(-1) and pushes r on the stack where r is the result of
        op (r, a = top(-1), b = top).  (Note the ordering!) */
{
  Element *a, *b, *c, *er, *ef;
  lia_push ((Lia_obj) NULL);  /* add an empty top; side effects on s! */
  if (not (s.top and s.top->next and s.top->next->next))
    basic_error ("lia_binop: underflow");
  a  = s.top->next;
  b  = a->next;
  c  = s.top;
  er = b->next;
  ef = s.free;
  op (c->lia_object, b->lia_object, a->lia_object);
  /* now:  s.top  == c --> a --> b --> er...;  s.free == ef... */
  c->next = er;
  b->next = ef;
  s.free = a; 
  /* and now:  s.top  == c --> r...;  s.free == a --> b --> ef ... */
  Assert ((s.free->next->next == ef) and (s.top->next == er));
}

/*--------------------------------------------------------------------------*/

void lia_negtop ()
     /* Changes sign of top element:
        stack[..., top] --> stack[..., -top] */
{
  lia_chs (s.top->lia_object);
}
