#ifndef __DIGIT_H__
#define __DIGIT_H__

class Digit
{
private:
    struct lcat {
	int layer;
	int cat;
    };
    std::vector<lcat> cats;

    DisplayDriver *display;

    int SetCategory(int, int);

public:
    Digit(DisplayDriver *);

    int InitCats();

    int AddLine(int, std::vector<double>, bool, int, int);
    int DeleteSelectedLines();

    int GetCategory(int);

};

#endif /* __DIGIT_H__ */

