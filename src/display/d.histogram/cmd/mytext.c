#ifdef R_text
#undef R_text
#endif
mytext(s)char*s;
{
	R_stabilize();
	R_text(s);
	R_stabilize();
}
