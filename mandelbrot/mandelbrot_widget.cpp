#include "mandelbrot_widget.h"
#include "ui_mandelbrot_widget.h"
#include <QPainter>
#include <QWheelEvent>
#include <iostream>
#include <thread>

mandelbrot_widget::mandelbrot_widget(QWidget *parent)
    : QMainWindow(parent),
      render_mutexes(thread_count),
      ui(new Ui::mandelbrot_widget)
{
    ui->setupUi(this);
    for (size_t tid = 0; tid < thread_count; tid++) {
        render_threads.push_back(std::make_unique<render_thread>(render_mutexes[tid], tid, thread_count));
        connect(&(*(render_threads[tid])), &render_thread::rendered, this, &mandelbrot_widget::update_image);
    }

}

mandelbrot_widget::~mandelbrot_widget() {
    stop_threads();
}

void mandelbrot_widget::paintEvent(QPaintEvent *ev) {
    QMainWindow::paintEvent(ev);
    QPainter painter(this);

    std::vector<std::unique_ptr<QMutexLocker>> lockers; // ЭТО Я ТАК ЛОЧУ МЬЮТЕКСЫ
    for (size_t tid = 0; tid < thread_count; tid++) {
        lockers.push_back(std::make_unique<QMutexLocker>(&(render_mutexes[tid])));
    }

    painter.drawImage(0, 0, image);
}

void mandelbrot_widget::keyPressEvent(QKeyEvent *ev) {
    switch (ev->key()) {
    case Qt::Key_Left:
        scroll(-scale, 0);
        break;
    case Qt::Key_Right:
        scroll(+scale, 0);
        break;
    case Qt::Key_Down:
        scroll(0, -scale);
        break;
    case Qt::Key_Up:
        scroll(0, +scale);
        break;
    }

}

void mandelbrot_widget::stop_threads() {
    for (auto & thread : render_threads) {
        thread->stop();
    }
}

void mandelbrot_widget::wait_threads() {
    for (auto & thread : render_threads) {
        thread->wait();
    }
}

void mandelbrot_widget::continue_threads_with_new_params() {
    finished_threads = 0;
    for (auto & thread : render_threads) {
        thread->render(image.bits(), center_offset, scale, size(), image.bytesPerLine());
    }
}

void mandelbrot_widget::scroll(double x, double y) {
    center_offset += std::complex<double>{20 * x, 20 * y};
    continue_threads_with_new_params();
}


void mandelbrot_widget::resizeEvent(QResizeEvent * /* event */) {
    stop_threads();
    wait_threads();

    image = QImage(size(), QImage::Format_RGB888);
    continue_threads_with_new_params();
}

void mandelbrot_widget::wheelEvent(QWheelEvent *event) {
    const int numDegrees = event->angleDelta().y() / 8;
    const double numSteps = numDegrees / double(15);
    scale *= (pow(0.8, numSteps));
    continue_threads_with_new_params();
}

void mandelbrot_widget::mousePressEvent(QMouseEvent *ev) {
    if (ev->button() == Qt::LeftButton) {
        last_drag = ev->pos();
    }
}

void mandelbrot_widget::mouseMoveEvent(QMouseEvent *event) {
    if (event->buttons() & Qt::LeftButton) {
       auto new_pos = event->pos();
       double x = new_pos.x() - last_drag.x();
       double y = new_pos.y() - last_drag.y();
       center_offset -= std::complex(x * scale, y * scale);
       last_drag = new_pos;
       continue_threads_with_new_params();
    }
}

void mandelbrot_widget::update_image() {
    finished_threads++;
    if (finished_threads >= thread_count / 2)
        update();
}

