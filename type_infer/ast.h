#include <string>
using std::string;

class Exp {
  public:
    virtual string show() const {return "";}
    virtual ~Exp() {}
};

class Ident : public Exp {
  public:
    Ident(const string& name): name_(name) {}
    string show() const {return name_;}
  private:
    const string& name_;
};

class Apply : public Exp {
  public:
    Apply(const Ident& fun, const Exp& arg): fun_(fun), arg_(arg) {}
    string show() const {return fun_.show() + "(" + arg_.show() + ")";}
  private:
    const Ident& fun_;
    const Exp& arg_;
};

class Lambda : public Exp {
  public:
    Lambda(const Ident& var, const Exp& body): var_(var), body_(body) {}
    string show() const {return "lambda " + var_.show() + " = " + body_.show();}
  private:
    const Ident& var_;
    const Exp& body_;
};

class Let : public Exp {
  public:
    Let(const Ident& var, const Exp& value, const Exp& body):
      var_(var), value_(value), body_(body) {}
    string show() const {return "let " + var_.show() + " = " + value_.show() + " in " + body_.show();}
  private:
    const Ident& var_;
    const Exp& value_;
    const Exp& body_;
};

