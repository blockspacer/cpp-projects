#pragma once

#include <QMainWindow>
#include "render_thread.h"
#include <atomic>
#include <QThread>
#include <vector>
#include <memory>

QT_BEGIN_NAMESPACE
namespace Ui { class mandelbrot_widget; }
QT_END_NAMESPACE

class mandelbrot_widget : public QMainWindow
{
    Q_OBJECT

public:
    mandelbrot_widget(QWidget *parent = nullptr);
    ~mandelbrot_widget();
    void wheelEvent(QWheelEvent*) override;
    void mousePressEvent(QMouseEvent *event) override;
    void mouseMoveEvent(QMouseEvent *event) override;
    void resizeEvent(QResizeEvent* ) override;
    void paintEvent(QPaintEvent*) override;
    void keyPressEvent(QKeyEvent*) override;

    void stop_threads();
    void wait_threads();
    void continue_threads_with_new_params();
    void scroll(double, double);

private slots:
    void update_image();

private:
    const size_t thread_count = QThread::idealThreadCount() - 1;

    std::vector<std::unique_ptr<render_thread>> render_threads;
    std::vector<QMutex> render_mutexes;

    double scale = 0.005;
    std::complex<double> center_offset = {-0.5, 0.};
    QPoint last_drag;

    QImage image;
    std::atomic<size_t> finished_threads = 0;

    std::unique_ptr<Ui::mandelbrot_widget> ui;
};
