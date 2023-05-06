\documentclass[10pt,aspectratio=43,mathserif]{beamer} 
%设置为 Beamer 文档类型，设置字体为 10pt，长宽比为16:9，数学字体为 serif 风格
\batchmode
\usepackage{graphicx}
\usepackage{animate}
%导入一些用到的宏包
\usepackage{amsmath,bm,amsfonts,amssymb,enumerate,epsfig,bbm,calc,color,ifthen,capt-of,multimedia,hyperref}
\usepackage{xeCJK} %导入中文包
\setCJKmainfont{SimHei} %中文字体采用黑体  Microsoft YaHei

%设置Beamer主题
\usepackage{hyperref}
\usetheme{Berlin} %主题
\usecolortheme{sustech} %主题颜色

%算法设置
\usepackage{algorithm}  
\usepackage{algorithmicx}  
\usepackage{algpseudocode}
\floatname{algorithm}{算法}
\renewcommand{\algorithmicrequire}{\textbf{输入:}} 
\renewcommand{\algorithmicensure}{\textbf{输出:}}  

\algrenewcommand{\algorithmiccomment}[1]{ $//$ #1}

%代码设置
\usepackage{fancybox}
\usepackage{xcolor}
\usepackage{times}
\usepackage{listings}

\definecolor{mygreen}{rgb}{0,0.6,0}
\definecolor{mygray}{rgb}{0.5,0.5,0.5}
\definecolor{mymauve}{rgb}{0.58,0,0.82}
\newcommand{\Console}{Console}
\lstset{ %
backgroundcolor=\color{white},   % choose the background color
basicstyle=\footnotesize\rmfamily,     % size of fonts used for the code
columns=fullflexible,
breaklines=true,                 % automatic line breaking only at whitespace
captionpos=b,                    % sets the caption-position to bottom
tabsize=4,
commentstyle=\color{mygreen},    % comment style
escapeinside={\%*}{*)},          % if you want to add LaTeX within your code
keywordstyle=\color{blue},       % keyword style
stringstyle=\color{mymauve}\ttfamily,     % string literal style
numbers=left, 
%	frame=single,
rulesepcolor=\color{red!20!green!20!blue!20},
% identifierstyle=\color{red},
language=c
}

%设置英文字体
\setsansfont{Microsoft YaHei}
\setmainfont{Microsoft YaHei}

%题目，作者，学校，日期
\title{Reinventing the Wheel: Publishing High-quality Slides}
\subtitle{\fontsize{9pt}{14pt}\textbf{利用公共网关的SMS生态系统的安全性描述}}
\author{答辩人: 李易峰 \newline \newline 指导老师: 吴亦凡教授}
\institute{中北大学英雄与联盟工程学院}
\date{\today}

%目录设置
\AtBeginSection[]
{
\begin{frame}<beamer>
\frametitle{\textbf{目录}}
\tableofcontents[currentsection]
\end{frame}
}
\beamerdefaultoverlayspecification{<+->}

\begin{frame}{五大主流机器学习框架的比较}
\begin{table}[htbp!]
\centering
\caption{主流机器学习框架}
\rowcolors{1}{mygray}{white}
\begin{tabular}{c|c|c|c|c}
\toprule[1pt] 
机器学习库	& 机构 & 支持语言  & 平台 & Tensor \\ 
\toprule[1pt] 
TensorFlow	& Google & C++，Python &跨平台 & Good \\ 
\hline 
Pytorch	&  Facebook& Python & 跨平台 & Good \\ 
\hline 
MXNet	&  Amazon& Most Lang. & Most Plat.  & No  \\ 
\hline 
Theano	& Montreal & Python & 跨平台  & Basic \\ 
\hline 
CNTK	& Microsoft &C++ & Win、Linux &  Unknown\\ 
\bottomrule[1pt] 
\end{tabular}
\end{table}

\section{背景}
\begin{frame}
最近兴趣使然写了几个Python库，也发布到了Pypi上，虽然没什么人下载，但自己在其他机器上用着也会很方便。这里我向大家介绍一下如何在Pypi上发表自己的Python库。
\lstinputlisting[lastline=10,
                 language=Python,
                 frame=single,
                 caption=First ten lines of some Python code,
                 label=python]
{ComputeSNR.py}
\end{frame}

\begin{algorithm}[H]
\caption{HOSVD}
\small 
\KwIn{HOSVD($\mathcal{X},R_{1},R_{2}.....R_{N}$)}
\KwOut{ $\mathcal{G},A_{(1)},A_{(2)}......A_{(N)} $}

\For{$k=1$ to $N$}
{
$A_{(n)}\leftarrow R_{n}$left singular matrix of $X_{(n)}$}

$\mathcal{G}=\leftarrow \mathcal{X} \times A_{(1)}^{T} \times A_{(2)}^{T}...... \times A_{(N)}^{T}$\\
\Return $\mathcal{G},A_{(1)},A_{(2)}......A_{(N)} $
\end{algorithm}

\section{如何在Pypi上发表自己的Python库}
\begin{frame}
\frametitle{动画演示}
\begin{center}
\animategraphics[controls, buttonsize=3mm, height=0.8\textheight]{24}{gif/13_}{1}{100}
\end{center}
\end{frame}
